let int32_in_string n s offs =
  let s_char_of_int n =
    char_of_int n
  in
    s.[offs + 3] <- s_char_of_int (Int32.to_int (Int32.shift_right n 24));
    let n = Int32.to_int n in
      s.[offs + 2] <- s_char_of_int ((n lsr 16) land 0xff);
      s.[offs + 1] <- s_char_of_int ((n lsr 8) land 0xff);
      s.[offs + 0] <- s_char_of_int (n land 0xff)

type vendor = Intel | AMD | Cyrix | Centaur | IDT | NatSemi | NexGen | RiSE | SiS | Transmeta | Unknown_vendor of string

type processor =
    {
      vendor_name : string; (** vendor string *)
      vendor : vendor;
      features : int32; (** features *)
      stepping : int;
      model : int;
      family : int;
      proc_type : int;
      ext_model : int;
      ext_family : int;
    }

(* TODO: handle smp, HT, ... *)

(** The currently detected cpu. *)
let proc = ref None

external get_freq : unit -> float = "caml_funk_cpu_get_freq"

let string_of_features f =
  let ans = ref "" in
  let pbe = (Int32.to_int (Int32.shift_right f 31) = 1) in
  let f = Int32.to_int f in
    (* add a feature *)
  let af bit name =
    let mask = 1 lsl bit in
      if f land mask = mask then
        ans := !ans ^ (if !ans = "" then "" else " ") ^ name
  in
    af 0 "fpu";
    af 1 "vme";
    af 2 "de";
    af 3 "pse";
    af 4 "tsc";
    af 5 "msr";
    af 6 "pae";
    af 7 "mce";
    af 8 "cxchg8";
    af 9 "apic";
    af 11 "sep";
    af 12 "mtrr";
    af 13 "pge";
    af 14 "mca";
    af 15 "cmov";
    af 16 "pat";
    af 17 "pse36";
    af 18 "psn";
    af 19 "clfl";
    af 21 "dtes";
    af 22 "acpi";
    af 23 "mmx";
    af 24 "fxsr";
    af 25 "sse";
    af 26 "sse2";
    af 27 "ss";
    af 28 "htt";
    af 29 "tm1";
    af 30 "ia-64";
    if pbe then ans := !ans ^ " pbe";
    !ans

let string_of_intel_signature family ext_family model stepping brand =
  match family with
    | 4 -> "i486" ^ begin
	(* family 4 *)
	match model with
	  | 0 -> " DX-25/33"
	  | 1 -> " DX-50"
	  | 2 -> " SX"
	  | 3 -> " DX2"
	  | 4 -> " SL"
	  | 5 -> " SX2"
	  | 7 -> " DX2-WB"
	  | 8 -> " DX4"
	  | 9 -> " DX4-WB"
	  | _ -> ""
      end
    | 5 -> "Pentium" ^ begin
	(* family 5 *)
	match model with
	  | 0 -> " A-step"
	  | 1 -> " 60/66"
	  | 2 -> " 75-200"
	  | 3 -> " Overdrive"
	  | 4 -> " MMX"
	  | 7 -> " Mobile"
	  | 8 -> " MMX Mobile"
	  | _ -> ""
      end
    | 6 -> begin
	(* family 6 *)
	match model with
	  | 0 -> "Pentium Pro A-step"
	  | 1 -> "Pentium Pro" ^ begin
	      (* family 6 model 1 *)
	      match stepping with
		| 1 -> " [B0]"
		| 2 -> " [C0]"
		| 6 -> " [sA0]"
		| 7 -> " [sA1]"
		| 9 -> " [sB1]";
		| _ -> ""
	    end
	  | 3 -> "Pentium II" ^ begin
	      (* family 6 model 3 *)
	      match stepping with
		| 2 -> " Overdrive [tdB0]"
		| 3 -> " (Klamath) [C0]"
		| 4 -> " (Klamath) [C1]";
		| _ -> ""
	    end
	  | 4 -> "Pentium II (Deschutes)"; (* Even Dave Jones is not sure here *)
	  | 5 -> begin
	      (* family 6 model 5 *)
	      (* if 6, 5 and L2 cache size = 0, "Celeron (Covington)" *)
	      (* if 6, 5 and L2 cache size = 256, "Mobile Pentium II (Dixon)" *)
	      (* if 6, 5 and L2 cache size = 512, the following *)
	      "Pentium II" ^ match stepping with
		| 0 -> " [dA0]"
		| 1 -> " (Deschutes) [dA1]"
		| 2 -> " (Deschutes) [dB0]"
		| 3 -> " (Deschutes) [dB1]"
		| _ -> ""
	    end
	  | 6 -> begin
	      (* family 6 model 6 *)
	      (* si L2 = 128, "Celeron (Mendocino)" *)
	      match stepping with
		| 0 -> "Celeron-A [mA0]"
		| 5 -> "Celeron-A [mB0]"
		| 0xA -> "Mobile Pentium II [mdA0]"
		| _ -> "Celeron / Mobile Pentium II"
	    end
	  | 7 -> begin
	      (* family 6 model 7 *)
	      match stepping with
		| 2 -> "Pentium III (Katmai) [kB0]"
		| 3 -> "Pentium III (Katmai) [kC0]"
		| _ -> "Pentium III/Pentium III Xeon"
	    end
	  | 8 -> begin
	      (* family 6 model 8 *)
	      match brand with
		| 2 -> "Pentium III-M  (Coppermine)" ^ begin
		    (* family 6 model 8 brand 2 *)
		    match stepping with
		      | 1 -> " [cA2]"
		      | 3 -> " [cB0]"
		      | 6 -> " [cC0]"
		      | 0xA -> " [cD0]"
		      | _ -> ""
		  end
		| 3  -> "Pentium III Xeon" ^ begin
		    (* family 6 model 8 brand 3 *)
		    match stepping with
		      | 2 -> " [A2]"
		      | 3 -> " [B0]"
		      | 6 -> " [C0]"
		      | _ -> ""
		  end
		| 8 -> "Mobile Pentium III"
		| _ -> begin
		    (* family 6 model 8 brand _ *)
		    (* Celeron instead of PIII if L2 cache size = 128 *)
		    "Pentium III (Coppermine)" ^ match stepping with
		      | 1 -> " [cA2]"
		      | 3 -> " [cB0]"
		      | 6 -> " [cC0]"
		      | 0xA -> " [cD0]"
		      | _ -> ""
		  end
	    end
	  | 9 -> "Pentium M (Banias)"
	  | 0xA -> begin
	      (* family 6 model 9 *)
	      match brand with
		| 0 -> "Pentium II (Deschutes)"
		| 1 -> "Celeron"
		| 2 -> "Pentium III"
		| 3 -> "Pentium III Xeon" ^ begin
		    (* family 6 model 8 brand 3 *)
		    match stepping with
		      | 0 -> " [A0]"
		      | 1 -> " [A1]"
		      | 4 -> " [B0]"
		      | _ -> ""
		  end
		| 4 -> "Pentium III (Cascades)"
		| _ -> "Unknown CPU"
	    end
	  | 0xB -> begin
	      (* family 6 model B *)
	      match brand with
		| 1
		| 3 -> "Celeron (Tualatin) [tA1/cA2]"
		| 6 -> "Pentium III-M"
		| _ -> begin
		    (* family 6 model B brand _ *)
		    match stepping with
		      | 1 -> "Pentium III [B-1]"
		      | 4 -> "Pentium M [B-1]"
		      | _ -> "Unknown CPU"
		  end
	    end
	  | 0xD -> "Pentium M" ^ begin
	      (* family 6 model D *)
	      match stepping with
		| 6 -> " (Dothan) [B-1]"
		| 8 -> " (Sonoma)"
		| _ -> ""
	    end
	  | _ -> "Unknown CPU"
      end
    | 7 -> "Itanium"
    | 0xF -> begin
	(* family F *)
	match ext_family with
	  | 0 -> begin
	      (* family F.0 *)
	      match model with
		| 0 -> "Pentium 4" ^ begin
		    (* family F.0 model 0 *)
		    match stepping with
		      | 7 -> "[B2]"
		      | 0xA -> "[C1]"
		      | _ -> ""
		  end
		| 1 -> "Pentium 4 (Willamette)" ^ begin
		    (* family F.0 model 1 *)
		    match stepping with
		      | 1 -> " [C0]"
		      | 2 -> " [D0]"
		      | 3 -> " [E0]"
		      | _ -> ""
		  end
		| 2 -> begin
		    (* family F.0 model 2 *)
		    begin
		      match brand with
			| 0xA -> "Celeron (P4 core)"
			| 7
			| _ -> "Pentium 4 (Northwood)"
		    end ^ begin
		      match stepping with
			| 4 -> " [B0]"
			| 5 -> " EE [M0]"
			| 7 -> " [C1]"
			| 9 -> " [D1]"
			| _ -> ""
		    end
		  end
		| 3 -> "Pentium 4 (Prescott)" ^ begin (* also Nocona (early EM64T Prescott Xeon) ? *)
		    (* family F.0 model 3 *)
		    match stepping with
		      | 3 -> " [C0]"
		      | 4 -> " [D0]"
		      | _ -> ""
		  end
		| 4 -> "Pentium 4 (Prescott)" ^ begin
		    (* family F.0 model 4 *)
		    match stepping with
		      | 1 -> " 570J [E0]"
		      | 3 -> " 630 EM64T [N0]" (* also 660 ? *)
		      | _ -> ""
		    end
		| 5 -> "Pentium 4 Xeon (Foster)"
		| _ -> "Unknown CPU"
	    end
	  | 1 -> "Itanium 2 (IA-64)"
	  | _ -> Printf.sprintf "Unknown extended family (%x)\n" ext_family
      end
    | _ -> Printf.sprintf "Unknown family (%x)\n" family

let string_of_amd_signature family ext_family model stepping brand =
  match family with
    | 4 -> begin
	(* family 4 *)
	match model with
	  | 3 -> "Am486DX2-WT"
	  | 7 -> "Am486DX2-WB"
	  | 8 -> "Am486DX4-WT / Am5x86-WT"
	  | 9 -> "Am486DX4-WB / Am5x86-WB"
	  | 0xA -> "Elan SC400"
	  | 0xE -> "Am5x86-WT"
	  | 0xF -> "Am5x86-WB"
	  | _ -> "Unknown CPU"
      end
    | 5 -> begin
	(* family 5 *)
	match model with
	| 0 -> "SSA5 (PR75/PR90/PR100)"
	| 1 -> "K5 (PR120/PR133)"
	| 2 -> "K5 (PR166)"
	| 3 -> "K5 (PR200)"
	| 6 -> "K6 (0.30 um)"
	| 7 -> "K6 (0.25 um)"
	| 8 -> "K6-2" ^ begin
	    (* family 5 model 8 *)
	    match stepping with
	      | n when n>=8 -> " (CXT core)"
	      | _ -> ""
	  end
	| 9 -> "K6-III"
	| 0xC -> "K6-2+ (0.18um)"
	| 0xD -> "K6-3+ (0.18um)"
	| _ -> "Unknown CPU"
      end
    | 6 -> begin
	(* family 6 *)
	match model with
	  | 0 -> "K7 ES"
	  | 1 -> "Athlon (0.25um)" ^ begin
	      (* family 6 model 1 *)
	      match stepping with
		| 1 -> " [C1]"
		| 2 -> " [C2]"
		| _ -> ""
	    end
	  | 2 -> "Athlon(0.18um)" ^ begin
	      (* family 6 model 2 *)
	      match stepping with
		| 1 -> " [A1]"
		| 2 -> " [A2]"
		| _ -> ""
	    end
	  | 3 -> "Duron (spitfire)" ^ begin
	      (* family 6 model 3 *)
	      match stepping with
		| 0 -> " [A0]"
		| 1 -> " [A2]"
		| _ -> ""
	    end
	  | 4 -> "Athlon (Thunderbird)" ^ begin
	      (* family 6 model 4 *)
	      match stepping with
		| 0 -> " [A1]"
		| 1 -> " [A2]"
		| 2 -> " [A4-8]"
		| 3 -> " [A9]"
		| _ -> ""
	    end
	  | 6 -> begin
	      (* family 6 model 6 *)
		(* if mobile "Mobile Athlon 4" *)
		(* if L2 size < 256, "Duron (Morgan)" *)
	      "Athlon (Palomino)" ^ match stepping with
		| 0 -> " [A0-A1]"
		| 1 -> " [A2]"
		| _ -> ""
	    end
	  | 7 -> begin
	      (* family 6 model 7 *)
	      (* if mobile, prefix with "Mobile " *)
	      "Duron (Morgan core)" ^ match stepping with
		| 0 -> " [A0]"
		| 1 -> " [A1]"
		| _ -> ""
	    end
	  | 8 -> begin
	      (* family 6 model 8 *)
	      (* if mobile, prefix with "Mobile " *)
	      (* L2 size < 256, "Duron" *)
	      "Athlon (Thoroughbred)" ^ match stepping with
		| 0 -> " [A0]"
		| 1 -> " [B0]"
		| _ -> ""
	    end
	  | 0xA -> begin
	      (* family 6 model 9 *)
	      (* if mobile, prefix with "Mobile " *)
	      "Athlon (Barton)"
	    end
	  | _ -> "Unknown CPU"
      end
    | 0xF -> begin
	(* family F *)
	match model with
	  | 0 -> "Athlon 64" ^ begin
	      (* family F model 0 *)
	      match stepping with
		| 0 -> " [SH7-A0]"
		| 1 -> " [SH7-A2]"
		| _ -> ""
	    end
	  | 1 -> "Opteron ES" ^ begin
	      (* family F model 1 *)
	      match stepping with
		| 0 -> " [SH7-A0]"
		| 1 -> " [SH7-A2]"
		| _ -> ""
	    end
	  | 4 -> "Athlon 64" ^ begin
	      (* family F model 4 *)
	      match stepping with
		| 0 -> " [SH7-B0]" (* " [SH8-D0]" if emodel <> 0 *)
		| 8 -> " [SH7-C0]"
		| 0xA -> " [SH7-CG]"
		| _ -> ""
	    end
	  | 5 -> "Opteron" ^ begin
	      (* family F model 5 *)
	      match stepping with
		| 0 -> " [SH7-B0]" (* " [SH8-D0]" if emodel <> 0 *)
		| 1 -> " [SH7-B3]"
		| 8 -> " [SH7-C0]"
		| 0xA -> " [SH7-CG]"
		| _ -> ""
	    end
	  | 7 -> "Athlon 64" ^ begin
	      (* family F model 7 *)
	      match stepping with
		| 0 -> " [SH8-D0]"
		| 0xA -> " [SH7-CG]"
		| _ -> ""
	    end
	  | 8 -> "Athlon 64" ^ begin
	      (* family F model 8 *)
	      match stepping with
		| 2 -> " CH7-CG"
		| _ -> ""
	    end
	  | 0xB -> "Athlon 64" ^ begin
	      (* family F model B *)
	      match stepping with
		| 2 -> " CH7-CG"
		| _ -> ""
	    end
	  | 0xC -> "Athlon 64" ^ begin
	      (* family F model C *)
	      match stepping with
		| 0 -> " DH7-CG"
		| _ -> ""
	    end
	  | 0xE -> "Athlon 64" ^ begin
	      (* family F model E *)
	      match stepping with
		| 0 -> " DH7-CG"
		| _ -> ""
	    end
	  | 0xF -> "Athlon 64" ^ begin
	      (* family F model F *)
	      match stepping with
		| 0 -> " DH7-CG"
		| _ -> ""
	    end
	  | _ -> "Unknown CPU"
      end
    | _ -> Printf.sprintf "Unknown family (%x)\n" family

let string_of_cyrix_signature family ext_family model stepping brand =
  match family with
    | 4 -> begin
	match model with
	  | 5 -> "MediaGX"
	  | _ -> "Unknown CPU"
      end
    | 5 -> begin
	match model with
	  | 2 -> begin
	      match stepping with
	      | 0 -> "6x86"
	      | 4 -> "GXm"
	      | _ -> "Unknown CPU"
	    end
	  | _ -> "Unknown CPU"
      end
    | 6 -> begin
	match model with
	  | 0 -> "6x86/MX"
	  | 2 -> "MII"
	  | _ -> "Unknown CPU"
      end
    | _ -> Printf.sprintf "Unknown family (%x)\n" family

let string_of_idt_signature family ext_family model stepping brand =
  match family with
    | 5 -> begin
	match model with
	  | 4 -> "Winchip C6"
	  | 8 -> "Winchip 2" ^ begin
	      match stepping with
		| n when n>=7 && n<=9 -> "A"
		| n when n >= 0xA && n <= 0xF -> "B"
		| _ -> ""
	    end
	  | 9 -> "Winchip 3"
	  | _ -> "Unknown CPU"
      end
    | 6 -> begin
	match model with
	  | 6 -> "VIA Cyrix 3 (Samuel) [C5A]"
	  | 7 -> "VIA C3" ^ begin
	      match stepping with
		| n when n<=7 -> " (Samuel 2) [C5B]"
		| _ -> " (Ezra) [C5C]"
	    end
	  | 8 -> "VIA C3 (Ezra-T) [C5M/C5N]"
	  | 9 -> "VIA C3 (Nehemiah) [C5XL]"
	  | _ -> "Unknown CPU"
      end
    | _ -> Printf.sprintf "Unknown family (%x)\n" family

let string_of_natsemi_signature family ext_family model stepping brand =
  match family with
    | 5 -> begin
	match model with
	  | 4 -> "Geode GX1"
	  | _ -> "Unknown CPU"
      end
    | _ -> Printf.sprintf "Unknown family (%x)\n" family

let string_of_rise_signature family ext_family model stepping brand =
  match family with
    | 5 -> begin
	match model with
	  | 0 -> "iDragon (0.25um)"
	  | 2 -> "iDragon (0.18um)"
	  | 8 -> "iDragon II (0.25um)"
	  | 9 -> "iDragon II (0.18um)"
	  | _ -> "Unknown CPU"
      end
    | _ -> Printf.sprintf "Unknown family (%x)\n" family

let string_of_sis_signature family ext_family model stepping brand =
 match family with
    | 5 -> begin
	match model with
	  | 0 -> begin
	      match stepping with
		| 5 -> "SiS55x"
		| _ -> "Unknown CPU"
	    end
	  | _ -> "Unknown CPU"
      end
    | _ -> Printf.sprintf "Unknown family (%x)\n" family

exception No_such_vendor

let vendor_of_string vendor =
  match vendor with
    | "GenuineIntel" -> Intel
    | "AuthenticAMD" -> AMD
    | "CentaurHauls" -> Centaur
    | "CyrixInstead" -> Cyrix
    (* |  -> IDT *) (* TODO *)
    | "Geode by NSC" -> NatSemi
    | "NexGenDriven" -> NexGen
    | "RiseRiseRise" -> RiSE
    | "SiS SiS SiS" -> SiS
    | "GenuineTMx86" -> Transmeta
    | _ -> raise No_such_vendor

let string_of_signature vendor family ext_family model stepping brand =
    match vendor with
      | Intel -> string_of_intel_signature family ext_family model stepping brand
      | AMD -> string_of_amd_signature family ext_family model stepping brand
      | Centaur -> "Centaur (???)" (* TODO *)
      | Cyrix -> string_of_cyrix_signature family ext_family model stepping brand
      | IDT -> string_of_idt_signature family ext_family model stepping brand
      | NatSemi -> string_of_natsemi_signature family ext_family model stepping brand
      | NexGen -> "NexGen (???)" (* TODO *)
      | RiSE -> string_of_rise_signature family ext_family model stepping brand
      | SiS -> string_of_sis_signature family ext_family model stepping brand
      | Transmeta -> "Transmeta (???)" (* TODO *)
      | Unknown_vendor v -> Printf.sprintf "Unknown vendor (%s)\n" v

let physical_id ebx1 =
  let smp_num_siblings = ((Int32.to_int ebx1) land 0xff0000) lsr 16 in
  let index_lsb = ref 0 in
  let index_msb = ref 0 in
    while ((smp_num_siblings lsr !index_lsb) land 1) != 0 do incr index_lsb done;
    while (smp_num_siblings land (0x40000000 lsr !index_msb)) != 0 do incr index_msb done;
    index_msb := 30 - !index_msb;
    if !index_lsb != !index_msb then incr index_msb;
    let initial_apic_id = ((Int32.to_int ebx1) lsr 24) land 0xff in
      initial_apic_id lsr !index_msb

let parse_model ebx0 ecx0 edx0 eax1 ebx1 =
  let vendor_string = String.create 12 in
  let signature = Int32.to_int eax1 in
  let stepping = signature land 0xf in
  let model = (signature lsr 4) land 0xf in
  let family = (signature lsr 8) land 0xf in
  let proc_type = (signature lsr 12) land 0xf in
  let ext_model = (signature lsr 16) land 0xf in
  let ext_family = (signature lsr 20) land 0xf in
  let brand = (Int32.to_int ebx1) land 0xf in
    int32_in_string ebx0 vendor_string 0;
    int32_in_string edx0 vendor_string 4;
    int32_in_string ecx0 vendor_string 8;
    vendor_string, vendor_of_string vendor_string, stepping, model, family, proc_type, ext_model, ext_family, brand

let check_model () =
  let eax0, ebx0, ecx0, edx0 = Funk.cpuid Int32.zero in
    (* highest_op, vendor1, vendor3, vendor2 *) (* TODO: check higest_op (eax0) *)
  let eax1, ebx1, ecx1, edx1 = Funk.cpuid Int32.one in
    (* signature, brand, msr, features *)
  let vendor_string, vendor, stepping, model, family, proc_type, ext_model, ext_family, brand = parse_model ebx0 ecx0 edx0 eax1 ebx1 in
    Funk.kprintf "CPU" "Vendor: %s, name: %s\n" vendor_string (string_of_signature vendor family ext_family model stepping brand);
    Funk.kprintf "CPU" "Stepping: %d, type: %d, family: %d.%d model: %d.%d\n" stepping proc_type family ext_family model ext_model;
    Funk.kprintf "CPU" "Frequency %d MHz\n" (int_of_float (get_freq ())); (* TODO: %f is not working *)
    Funk.kprintf "CPU" "Features: %s\n" (string_of_features edx1);
    Funk.kprintf "CPU" "Physical ID: %d\n" (physical_id ebx1);
    proc :=
      Some {
        vendor_name = vendor_string;
        vendor = vendor;
        features = ebx1;
        stepping = stepping;
        model = model;
        family = family;
        proc_type = proc_type;
        ext_model = ext_model;
        ext_family = ext_family;
      }
