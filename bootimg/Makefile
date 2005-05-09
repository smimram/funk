 # makefile [part of the funk project]
#[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 # contents    : makefile for the funk kernel qemu test
 # copyright   : (C) 2005 by samuel mimram, samuel thibault
 # email       : samuel.mimram@ens-lyon.org, samuel.thibault@ens-lyon.org

###############################################################################
#                                                                             #
# This program is free software; you can redistribute it and/or               #
# modify it under the terms of the GNU General Public License                 #
# as published by the Free Software Foundation; either version 2              #
# of the License, or (at your option) any later version.                      #
#                                                                             #
# This program is distributed in the hope that it will be useful,             #
# but WITHOUT ANY WARRANTY; without even the implied warranty of              #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the                #
# GNU General Public License for more details.                                #
#                                                                             #
# You should have received a copy of the GNU General Public License           #
# along with this program; if not, write to the Free Software                 #
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. #
#                                                                             #
###############################################################################

VERSION	=	$(shell cat ../VERSION)
SRC	=	../src
KV	=	funk-i686-$(VERSION)
KERNEL	=	$(SRC)/kernel/$(KV)
IMG	=	funk.img
BASEIMG =	base.img.gz
QEMU	=	qemu
BOCHS	=	bochs

-include Makefile.conf

.PHONY: all clean qemu qemugdb bochs bochsgdb updateimg kernel

all: qemu

clean:
	rm -rf $(IMG)

qemu: $(IMG)
	$(QEMU) -dummy-net -fda funk.img -hda funk.img -boot a -m 16M

qemugdb: $(IMG)
	$(QEMU) -dummy-net -fda funk.img -hda funk.img -boot a -m 16M -s &
	sleep 1 
	xterm -e gdb $(KERNEL) -x gdbinit

qemu-x86_64: $(IMG)
	$(QEMU)-system-x86_64 -dummy-net -fda funk.img -hda funk.img -boot a -m 16M

qemulogs: $(IMG)
	$(QEMU) -dummy-net -fda funk.img -boot a -m 16M \
	        -d out_asm,in_asm,op,op_opt,int,exec,cpu,pcall

qemuconsole: $(IMG)
	$(QEMU) -dummy-net -fda funk.img -hda funk.img -boot a -m 16M -nographic

qemunet: $(IMG)
	$(QEMU) -fda funk.img -hda funk.img -boot a -m 16M \
		-macaddr 00:11:22:33:44:55 -user-net -redir tcp:6666::23

bochs: $(IMG)
	$(BOCHS) -f bochsrc

bochsgdb: $(IMG)
	$(BOCHS) -f bochsrc &
	sleep 2 
	xterm -e gdb $(KERNEL) -x gdbinit

updateimg: $(IMG)

$(IMG): $(KERNEL) $(BASEIMG)
	gunzip < $(BASEIMG) > $(IMG)
	mcopy -i $(IMG) $(KERNEL) ::funk || (rm -f $(IMG); false)

kernel: $(KERNEL)

$(KERNEL):
	make -C $(SRC)

livecd: $(IMG)
	mkisofs -b $(IMG) -o funk_live.iso $(IMG)
