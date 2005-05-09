let kprintf f = Funk.kprintf "KBD" f

type special_key =
  | F1
  | F2
  | Page_up
  | Page_down
  | Left
  | Right
  | Up
  | Down

type chr = Char of char | Special_key of special_key

type keyboard_state =
    {
      mutable ks_shift : bool;
      mutable ks_alt : bool;
      mutable ks_altgr : bool;
      mutable ks_ctrl : bool;
    }

let keyb_state =
  {
    ks_shift = false;
    ks_alt = false;
    ks_altgr = false;
    ks_ctrl = false;
  }

let key_handler = ref (fun _ _ -> ())

let on_key f =
  key_handler := f

let simulate_key c s =
  !key_handler c s
                   
let input_map = ref Inputmap_fr_latin1.input_map

let keyboard_status_port = 0x64

let keyboard_read_port = 0x60

exception No_key

let get_scancode () =
  if Funk.inb_p keyboard_status_port land 0x01 <> 0x01 then
    raise No_key;
  Funk.inb_p keyboard_read_port

let receive_scancode c =
  let return c =
    !key_handler c keyb_state
  in
    if c = 0xe0 then
      (
        let c = get_scancode () in
          if c = 0x38 then
            keyb_state.ks_altgr <- true
          else if c = 0x38 + 0x80 then
            keyb_state.ks_altgr <- false
          else if c = 0x1d then
            keyb_state.ks_ctrl <- true
          else if c = 0x1d + 0x80 then
            keyb_state.ks_ctrl <- false
          else if c = 0x49 then
            return (Special_key Page_up)
          else if c = 0x51 then
            return (Special_key Page_down)
          else if c = 0x4b then
            return (Special_key Left)
          else if c = 0x4d then
            return (Special_key Right)
          else if c = 0x48 then
            return (Special_key Up)
          else if c = 0x50 then
            return (Special_key Down)
      )
    else if c = 0x2a || c = 0x36 || c = 0x2a + 0x80 || c = 0x36 + 0x80 || c = 0x3a then
      keyb_state.ks_shift <- not keyb_state.ks_shift
    else if c = 0x38 then
      keyb_state.ks_alt <- true
    else if c = 0x38 + 0x80 then
      keyb_state.ks_alt <- false
    else if c = 0x1d then
      keyb_state.ks_ctrl <- true
    else if c = 0x1d + 0x80 then
      keyb_state.ks_ctrl <- false
    else if c = 0x3b then
      return (Special_key F1)
    else if c = 0x3b then
      return (Special_key F2)
    else if c land 0x80 = 0 then
      let map = (if keyb_state.ks_shift then 1 else 0) + (if keyb_state.ks_alt then 2 else 0) in
      let chr =
        let chr = !input_map.(map).(c) in
          (* if keyb_state.ks_ctrl then
            char_of_int ((int_of_char chr) land 0x1f)
          else *)
          chr
      in
        if keyb_state.ks_altgr then
          kprintf "altgr\n"
        else
          (
            if chr = '\000' then
              kprintf "Unknown scancode %x.\n" c
            else
              return (Char chr)
          )

let _ =
  Callback.register "funk_rcv_scancode" receive_scancode

let poll_keyboard () =
  while true
  do
    Irq.wait 1;
    (
      try
        while true
        do
          receive_scancode (get_scancode ())
        done
      with No_key -> ()
    )
  done

let init () =
  ignore (KThread.create poll_keyboard ());
  kprintf "Polling keyboard.\n%!"
