#!/usr/bin/perl -w

# gen_pci_ids.pl [part of the funk project]
#[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
# contents    : pci_ids generator
# copyright   : (C) 2005 by samuel mimram, samuel thibault,
#               nicolas guenot
# email       : samuel.mimram@ens-lyon.org, samuel.thibault@ens-lyon.org,
#               nicolas.guenot@ens-lyon.org

#******************************************************************************
#                                                                             *
# This program is free software; you can redistribute it and/or               *
# modify it under the terms of the GNU General Public License                 *
# as published by the Free Software Foundation; either version 2              *
# of the License, or (at your option) any later version.                      *
#                                                                             *
# This program is distributed in the hope that it will be useful,             *
# but WITHOUT ANY WARRANTY; without even the implied warranty of              *
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the                *
# GNU General Public License for more details.                                *
#                                                                             *
# You should have received a copy of the GNU General Public License           *
# along with this program; if not, write to the Free Software                 *
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. *
#                                                                             *
#*****************************************************************************)

use strict;

open IDS, "/usr/share/misc/pci.ids" or die "Cannot open pci.ids: $!\nCancelling regeneration of pci_ids.ml";

system("chmod +w pci_ids.ml");
open OUT, ">pci_ids.ml" or die "pci_ids.ml: $!";

print OUT "(* THIS FILE WAS AUTOMATICALLY GENERATED, PLEASE DO NOT EDIT. *)\n\n";
print OUT "let ids = \"";
# replace with this line to put pci.ids in a file
#print OUT "let _ = Filecmds.cat_to_file \"pci.ids\" \"";

while (<IDS>)
{
    #chomp;
    s/"/\\"/g;
    #s/\t/\\t/g;
    print OUT $_;
}

print OUT "\"\n\n";

close OUT;
close IDS;

system("cat pci_ids.ml.in >> pci_ids.ml");
system("chmod -w pci_ids.ml");
