;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016-2018, 2021-2023 Marius Bakke <marius@gnu.org>
;;; Copyright © 2016, 2017 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2016, 2017 David Craven <david@craven.ch>
;;; Copyright © 2017, 2018, 2020, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 nee <nee@cock.li>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2018, 2019, 2020 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2020, 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2021 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2022 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2021 Stefan <stefan-guix@vodafonemail.de>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (me packages bootloaders)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex))

(define source-with-patches
  (@@ (gnu packages linux) source-with-patches))

(define %argon-patch-1
  (origin
    (method url-fetch)
    ;; guix download https://aur.archlinux.org/cgit/aur.git/plain/argon_1.patch?h=grub-improved-luks2-git -o ~/Downloads/argon_1.patch 
    (uri "https://aur.archlinux.org/cgit/aur.git/plain/argon_1.patch?h=grub-improved-luks2-git")
    (sha256 (base32
             "1pydcjy2cjnb4zxcqr41hr34fg8alph314xasdsfvdw4zaz55s6h"))))

(define %argon-patch-2
  (origin
    (method url-fetch)
    ;; guix download https://aur.archlinux.org/cgit/aur.git/plain/argon_2.patch?h=grub-improved-luks2-git -o ~/Downloads/argon_2.patch 
    (uri "https://aur.archlinux.org/cgit/aur.git/plain/argon_2.patch?h=grub-improved-luks2-git")
    (sha256 (base32
             "1pydcjy2cjnb4zxcqr41hr34fg8alph314xasdsfvdw4zaz55s6h"))))

(define %argon-patch-3
  (origin
    (method url-fetch)
    ;; guix download https://aur.archlinux.org/cgit/aur.git/plain/argon_3.patch?h=grub-improved-luks2-git -o ~/Downloads/argon_3.patch 
    (uri "https://aur.archlinux.org/cgit/aur.git/plain/argon_3.patch?h=grub-improved-luks2-git")
    (sha256 (base32
             "1pydcjy2cjnb4zxcqr41hr34fg8alph314xasdsfvdw4zaz55s6h"))))

(define %argon-patch-4
  (origin
    (method url-fetch)
    ;; guix download https://aur.archlinux.org/cgit/aur.git/plain/argon_4.patch?h=grub-improved-luks2-git -o ~/Downloads/argon_4.patch 
    (uri "https://aur.archlinux.org/cgit/aur.git/plain/argon_4.patch?h=grub-improved-luks2-git")
    (sha256 (base32
             "1pydcjy2cjnb4zxcqr41hr34fg8alph314xasdsfvdw4zaz55s6h"))))

(define %argon-patch-5
  (origin
    (method url-fetch)
    ;; guix download https://aur.archlinux.org/cgit/aur.git/plain/argon_5.patch?h=grub-improved-luks2-git -o ~/Downloads/argon_5.patch 
    (uri "https://aur.archlinux.org/cgit/aur.git/plain/argon_5.patch?h=grub-improved-luks2-git")
    (sha256 (base32
             "1pydcjy2cjnb4zxcqr41hr34fg8alph314xasdsfvdw4zaz55s6h"))))

(define %grub-install_luks2-patch
  (origin
    (method url-fetch)
    ;; guix download https://aur.archlinux.org/cgit/aur.git/plain/grub-install_luks2.patch?h=grub-improved-luks2-git -o ~/Downloads/grub-install_luks2.patch 
    (uri "https://aur.archlinux.org/cgit/aur.git/plain/grub-install_luks2.patch?h=grub-improved-luks2-git")
    (sha256 (base32
             "1pydcjy2cjnb4zxcqr41hr34fg8alph314xasdsfvdw4zaz55s6h"))))

(define-public grub-efi-luks2-source
  (source-with-patches (package-source grub-efi)
                       (list %argon-patch-1
                             %argon-patch-2
                             %argon-patch-3
                             %argon-patch-4
                             %argon-patch-5
                             %grub-install_luks2-patch)))

(define-public grub-efi-luks2
  (package
    (inherit grub-efi)
    (name "grub-efi-luks2")
    (source grub-efi-luks2-source)
    (synopsis "GRand Unified Boot loader (with Argon2 and better LUKS2 support)")))
