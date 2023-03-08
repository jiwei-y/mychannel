;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019, 2020, 2021 Peng Mei Yu <pengmeiyu@riseup.net>
;;; Copyright © 2020 kanichos <kanichos@yandex.ru>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021 Songlin Jiang <hollowman@hollowman.ml>
;;; Copyright © 2021 Taiju HIGASHI <higashi@taiju.info>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023 Luis Felipe López Acevedo <luis.felipe.la@protonmail.com>
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

(define-module (me packages ibus)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public ibus-typing-booster
  (package
    (name "ibus-typing-booster")
    (version "2.22.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/mike-fabian/ibus-typing-booster/releases/download/"
                    version "/ibus-typing-booster-" version ".tar.gz"))
              (sha256
               ;; guix download https://github.com/mike-fabian/ibus-typing-booster/releases/download/2.22.1/ibus-typing-booster-2.22.1.tar.gz -o ~/Downloads/ibus-typing-booster-2.22.1.tar.gz
               ;; rm -rf ~/Downloads/ibus-typing-booster-2.22.1.tar.gz
               (base32
                "17p1iyc8av3zabps0168i7d5hd95r3wyf9whjx607mzvp67jvk0g"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/libexec/ibus-setup-typing-booster")
               `("LD_LIBRARY_PATH" ":" prefix
                 (,(string-append (assoc-ref inputs "m17n-lib") "/lib"))))
             #t))
         (delete 'check))))
    (native-inputs
     (list clang
           pkg-config
           gobject-introspection))
    (inputs
     (list python
           gtk+
           ibus
           m17n-lib))
    (synopsis "A completion input method for faster typing")
    (description "Ibus-typing-booster is a completion input method to speed-up typing.

The project was started in 2010 for Fedora 15. The original purpose was to make typing of Indic languages easier and faster by providing completion and spell checking suggestions.

Originally it was forked from ibus-table whose developer was Yu Yuwei acevery@gmail.com, with contributions from Caius(\"kaio\") chanceme@kaio.net.

Since then ibus-typing-booster has been improved to support many other languages as well (i.e. most languages except Chinese and Japanese are supported).

Recently the capability to type different languages at the same time without having to switch between languages has been added.")
    (home-page "https://github.com/mike-fabian/ibus-typing-booster")
    (license gpl3+)))

(define-public ibus-mozc
  (package
    (name "ibus-mozc")
    (version "20230303")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/mozc")
                    (commit "664d0c51ac17860dcfdbc0d005e904554806a660")))
              (sha256
               ;; git clone --depth 1 https://github.com/google/mozc ~/Downloads/mozc && guix hash --serializer=nar -x ~/Downloads/mozc && rm -rf ~/Downloads/mozc
               (base32 "1h9d7a7kwb07a5vf8cs40x25l4g650ahcd3q72wrjklld30fc0hb"))))
    (build-system python-build-system)
    (arguments
     `(#:use-setuptools? #f
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
;         (add-after 'unpack 'symlink
;           (lambda* (#:key inputs #:allow-other-keys)
;             (let ((gyp (assoc-ref inputs "python-gyp")))
;               (rmdir "src/third_party/gyp/")
;               (symlink gyp "src/third_party/gyp"))))
         (add-after 'unpack 'remove-clang
         ;; do some harden which we can't do in extra options
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/gyp/common.gypi"
               (("-lc++") 
               "-lstdc++"))))
         (add-after 'remove-clang 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((gyp (assoc-ref inputs "python-gyp"))
                   (out (assoc-ref outputs "out")))
               ;; (chdir "src")
               (add-installed-pythonpath inputs outputs)
               (setenv "GYP_DEFINES" 
                        (string-append
                          "\"" "document_dir=" out "/share/doc/mozc"
                          "use_libzinnia=1"
                          "use_libprotobuf=1"
                          "ibus_mozc_path=" out "/lib/ibus-mozc/ibus-engine-mozc"
                          "ibus_mozc_icon_path=" out "/share/ibus-mozc/product_icon.png"
                          "mozc_dir=" out "/lib/mozc"
                          "mozc_icons_dir=" out "/share/icons/mozc"
                          "ibus_component_dir=" out "/share/ibus/component"
                          "ibus_mozc_install_dir=" out "/share/ibus-mozc"
                          "emacs_helper_dir=" out "/bin"
                          "emacs_client_dir=" out "/share/emacs/site-lisp/emacs-mozc" "\""))
                       (invoke "python" "src/build_mozc.py" "gyp"
                               (string-append "--gypdir=" gyp "/bin")
                               (string-append "--server_dir="
                                              out "/lib/mozc")
                               "--target_platform=Linux"))))
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "python" "src/build_mozc.py" "build" "-c" "Release"
                     "unix/ibus/ibus.gyp:ibus_mozc"
                     "unix/emacs/emacs.gyp:mozc_emacs_helper"
                     "server/server.gyp:mozc_server"
                     "gui/gui.gyp:mozc_tool"
                     "renderer/renderer.gyp:mozc_renderer"
                     "--use_gyp_for_ibus_build")))
         (delete 'check)
;         (replace 'install
;           (lambda* (#:key inputs outputs #:allow-other-keys)
;             (let* (((out (assoc-ref outputs "out")))
;             (add-installed-pythonpath inputs outputs)
;             (setenv (string-append "PREFIX=" out))
;             (invoke "install" "-d"
;                     (string-append out "/share/licenses/ibus-mozc"))))))
      )))
    (inputs
      (list protobuf
            ibus
            gtk+-2
            libxcb
            qtbase-5
            zinnia))
    (native-inputs
      (list python
            python-six
            python-gyp
            ninja
            pkg-config))
    (synopsis "A Japanese Input Method Editor designed for multi-platform")
    (description
     "Mozc is a Japanese Input Method Editor (IME) designed for multi-platform such as Android OS, Apple OS X, Chromium OS, GNU/Linux and Microsoft Windows. This OpenSource project originates from Google Japanese Input.")
    (home-page "https://github.com/google/mozc")
    (license bsd-3)))