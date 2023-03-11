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
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages llvm)
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
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-programs
            (lambda* (#:key inputs #:allow-other-keys)
              (for-each
               (lambda (prog)
                 (wrap-program (string-append #$output prog)
                   `("GUIX_PYTHONPATH" ":" prefix
                     (,(getenv "GUIX_PYTHONPATH")))
                   `("GI_TYPELIB_PATH" ":" prefix
                     (,(getenv "GI_TYPELIB_PATH")
                      ,(string-append #$output "/lib/girepository-1.0")))
                   `("LD_LIBRARY_PATH" ":" prefix
                     (,(string-append (assoc-ref inputs "m17n-lib") "/lib")))
                   `("DICPATH" ":" prefix
                     (,(string-append (assoc-ref inputs "hunspell-dict-fr-moderne") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-pl") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-de") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-hu") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-he-il") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-it-it") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-en") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-en-au") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-en-ca") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-en-gb") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-en-gb-ize") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-en-us") "/share/hunspell")))
                    ))
               '("/bin/emoji-picker"
                 "/libexec/ibus-engine-typing-booster"
                 "/libexec/ibus-setup-typing-booster"))))
          (delete 'check))))
    (native-inputs
     (list pkg-config
           gobject-introspection))
    (inputs
     (list python
           python-dbus
           python-pygobject
           gtk+
           ibus
           hunspell-dict-fr-classique
           hunspell-dict-fr-moderne
           hunspell-dict-fr-réforme-1990
           hunspell-dict-fr-toutes-variantes
           hunspell-dict-pl
           hunspell-dict-de
           hunspell-dict-hu
           hunspell-dict-he-il
           hunspell-dict-it-it
           hunspell-dict-en
           hunspell-dict-en-au
           hunspell-dict-en-ca
           hunspell-dict-en-gb
           hunspell-dict-en-gb-ize
           hunspell-dict-en-us
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
                    (commit "664d0c51ac17860dcfdbc0d005e904554806a660")
                    (recursive? #t)))
              (sha256
               ;; git clone --depth 1 --recurse-submodules https://github.com/google/mozc ~/Downloads/mozc && guix hash --serializer=nar -x ~/Downloads/mozc && rm -rf ~/Downloads/mozc
               (base32 "0kp5xi02qv7xyfrsvm4b22ap8q93l7bgwlqb3njf5czz08q20bg0"))))
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
         (add-after 'unpack 'fix-gpp
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs "gcc-toolchain")))
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-append gcc "/include/c++:"
                                      gcc "/include:"
                                      gcc "/include/c++/x86_64-unknown-linux-gnu:"
                                      (getenv "CPLUS_INCLUDE_PATH"))))))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((gyp (assoc-ref inputs "python-gyp"))
                   (out (assoc-ref outputs "out")))
               ;; (chdir "src")
               (add-installed-pythonpath inputs outputs)
               (setenv "GYP_DEFINES" 
                        (string-append
                          "document_dir=" out "/share/doc/mozc" " "
                          "use_libzinnia=1" " "
                          "use_libprotobuf=1" " "
                          "ibus_mozc_path=" out "/libexec/ibus-mozc/ibus-engine-mozc" " "
                          "ibus_mozc_icon_path=" out "/share/ibus-mozc/product_icon.png" " "
                          "mozc_dir=" out "/libexec/mozc" " "
                          "mozc_icons_dir=" out "/share/icons/mozc" " "
                          "ibus_component_dir=" out "/share/ibus/component" " "
                          "ibus_mozc_install_dir=" out "/share/ibus-mozc" " "
                          "emacs_helper_dir=" out "/bin" " "
                          "emacs_client_dir=" out "/share/emacs/site-lisp/emacs-mozc"))
                       (invoke "python" "src/build_mozc.py" "gyp"
                               (string-append "--gypdir=" gyp "/bin")
                               (string-append "--server_dir="
                                              out "/libexec/mozc")
                               "--target_platform=Linux"))))
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "python" "src/build_mozc.py" "build" "-c" "Release"
                     "unix/ibus/ibus.gyp:ibus_mozc"
                     "unix/emacs/emacs.gyp:mozc_emacs_helper"
                     "server/server.gyp:mozc_server"
                     "gui/gui.gyp:mozc_tool"
                     ;"renderer/renderer.gyp:mozc_renderer"
                     "--use_gyp_for_ibus_build")))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (ibus_mozc_exec_dir (string-append out "/libexec/ibus-mozc"))
                    (ibus_component_dir (string-append out "/share/ibus/component"))
                    (ibus_mozc_install_dir (string-append out "/share/ibus-mozc"))
                    (mozc_dir (string-append out "/libexec/mozc")))
               (add-installed-pythonpath inputs outputs)
               (rename-file "src/out_linux/Release/ibus_mozc" "src/out_linux/Release/ibus-engine-mozc")
               (for-each (lambda (name)
                           (install-file name ibus_mozc_exec_dir))
                         '("src/out_linux/Release/ibus-engine-mozc"))
               (for-each (lambda (name)
                           (install-file name ibus_component_dir))
                         '("src/out_linux/Release/gen/unix/ibus/mozc.xml"))
               (rename-file "src/data/images/unix/ime_product_icon_opensource-32.png" "src/data/images/unix/product_icon.png")
               (rename-file "src/data/images/unix/ui-tool.png" "src/data/images/unix/tool.png")
               (rename-file "src/data/images/unix/ui-properties.png" "src/data/images/unix/properties.png")
               (rename-file "src/data/images/unix/ui-dictionary.png" "src/data/images/unix/dictionary.png")
               (rename-file "src/data/images/unix/ui-direct.png" "src/data/images/unix/direct.png")
               (rename-file "src/data/images/unix/ui-hiragana.png" "src/data/images/unix/hiragana.png")
               (rename-file "src/data/images/unix/ui-katakana_half.png" "src/data/images/unix/katakana_half.png")
               (rename-file "src/data/images/unix/ui-katakana_full.png" "src/data/images/unix/katakana_full.png")
               (rename-file "src/data/images/unix/ui-alpha_half.png" "src/data/images/unix/alpha_half.png")
               (rename-file "src/data/images/unix/ui-alpha_full.png" "src/data/images/unix/alpha_full.png")
               (for-each (lambda (name)
                           (install-file name ibus_mozc_install_dir))
                         '("src/data/images/unix/product_icon.png"
                           "src/data/images/unix/tool.png"
                           "src/data/images/unix/properties.png"
                           "src/data/images/unix/dictionary.png"
                           "src/data/images/unix/direct.png"
                           "src/data/images/unix/hiragana.png"
                           "src/data/images/unix/katakana_half.png"
                           "src/data/images/unix/katakana_full.png"
                           "src/data/images/unix/alpha_half.png"
                           "src/data/images/unix/alpha_full.png"))
               (for-each (lambda (name)
                           (install-file name mozc_dir))
                         '("src/out_linux/Release/mozc_server"
                           "src/out_linux/Release/mozc_tool"
                           "src/out_linux/Release/mozc_emacs_helper")))))
         (add-after 'install 'wrap
           (lambda* (#:key outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/libexec/ibus-mozc/ibus-engine-mozc")
               `("GUIX_PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH")))
               ;`("GI_TYPELIB_PATH" ":" prefix (,(getenv "GI_TYPELIB_PATH")))
              )
             #t))
         (delete 'check)
      )))
    (inputs
      (list protobuf
            ibus
            gtk+-2
            libxcb
            qtbase-5
            zinnia))
    (native-inputs
      (list clang
            gcc-toolchain
            python
            python-six
            python-gyp
            ninja
            pkg-config))
    (synopsis "A Japanese Input Method Editor designed for multi-platform")
    (description
     "Mozc is a Japanese Input Method Editor (IME) designed for multi-platform such as Android OS, Apple OS X, Chromium OS, GNU/Linux and Microsoft Windows. This OpenSource project originates from Google Japanese Input.")
    (home-page "https://github.com/google/mozc")
    (license bsd-3)))
