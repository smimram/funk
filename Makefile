 # makefile [part of the funk project]
#[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 # contents    : global makefile for funk
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

VERSION = $(shell cat VERSION)

ifneq ($(V),1)
	MAKEFLAGS += --no-print-directory --silent
endif

DISTFILES = AUTHORS ChangeLog COPYING Makefile README README.Devel TODO VERSION \
		$(wildcard bootimg/*.img*) bootimg/Makefile bootimg/bochsrc* bootimg/gdbinit \
		src/Makefile \
		$(wildcard doc/*) \
		src/kernel/Makefile $(wildcard src/kernel/*.ld) \
		$(wildcard src/include/*.ml src/include/*.mli) \
		$(wildcard src/kernel/mk/*.ml src/kernel/mk/*.mli src/kernel/mk/*.mll src/kernel/mk/*.mly src/kernel/mk/*.c) src/userland/gen_pci_ids.pl \
		$(wildcard src/kernel/run/*.c) \
		src/kernel/run/i686 $(wildcard src/kernel/run/i386/*.S src/kernel/run/i386/*.c src/kernel/run/i386/*.h) \
		$(wildcard src/kernel/run/include/*.h) \
		$(wildcard src/servers/*.ml src/servers/*.mli) \
		$(wildcard src/userland/*.ml src/userland/*.mli)

.PHONY: doc dist ChangeLog

all doc clean debug:
	@$(MAKE) RELDIR=$(RELDIR)src/ -C src $@

int:
	@$(MAKE) RELDIR=$(RELDIR)src/ -C src int
	@$(MAKE) qemu

qemu qemugdb qemu-x86_64 bochs bochsgdb qemulogs qemuconsole qemunet livecd: all
	@$(MAKE) -C bootimg $@

dist: $(DISTFILES) doc
	@mkdir funk-$(VERSION)
	@cp  -r --no-dereference --parents $(DISTFILES) funk-$(VERSION)
	@tar jcvf funk-$(VERSION).tar.bz2 funk-$(VERSION)
	@rm -rf funk-$(VERSION)

ChangeLog:
	@svn log > ChangeLog

dist-regression: dist
	@cp funk-$(VERSION).tar.bz2 /tmp
	@cd /tmp; tar jxvf funk-$(VERSION).tar.bz2; cd funk-$(VERSION); make
