(define-module (me packages qt)
  #:use-module (guix build-system qt)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg))

(define-public kvantum
  (package
    (name "kvantum")
    (version "1.0.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/tsujan/Kvantum")
              (commit (string-append "V" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1bzrh7pbb83lk53ryxb2v01lf7z16cdgybs31mh9490rhl5yp4yz"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f
       #:phases
        (modify-phases %standard-phases
            (replace 'configure
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out")))
                  (chdir "Kvantum")
                  (substitute* (find-files "." "\\.pro$")
                    (("PREFIX = /usr")
                      (string-append "PREFIX = "
                                     out)))
                  (substitute* "style/style.pro"
                    (("\\$\\$\\[QT_INSTALL_PLUGINS\\]")
                      (string-append out "/lib/qt$${QT_MAJOR_VERSION}/plugins")))
                  (invoke "qmake")))))))
    (native-inputs
     (list extra-cmake-modules pkg-config qttools-5))
    (inputs
     (list qtsvg-5
           libx11
           libxext
           kwindowsystem
           qtbase-5
           qtx11extras))
    (propagated-inputs (list qtwayland-5))
    (home-page "https://github.com/tsujan/Kvantum")
    (synopsis "A Linux SVG-based theme engine for Qt and KDE")
    (description "Kvantum (by Pedram Pourang, a.k.a. Tsu Jan) is an SVG-based theme engine for Qt, tuned to KDE and LXQt, with an emphasis on elegance, usability and practicality. Its homepage is https://github.com/tsujan/Kvantum.")
    (license license:gpl3+)))