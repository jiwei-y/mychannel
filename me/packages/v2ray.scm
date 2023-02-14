(define-module (me packages v2ray)
  #:use-module (gnu packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system qt)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix modules)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages curl))

(define-public v2ray
  (package
    (name "v2ray")
    (version "5.1.0")
    (source (origin
              (method url-fetch)
              ;; https://github.com/v2fly/v2ray-core/releases/download/v5.1.0/v2ray-linux-64.zip
              (uri (string-append
                    "https://github.com/v2fly/v2ray-core/releases/download/v"
                    version "/v2ray-linux-64.zip"))
              (sha256
               (base32
                "0slfa8bdphhhwwd2ycjnjl7lp549x5wjvwd1ymhp7crl2x536w76"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan '(("v2ray" "/bin/")
                        ("geoip.dat" "/bin/")
                        ("geosite.dat" "/bin/"))
       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key source #:allow-other-keys)
                      (invoke "unzip" source))))))
    (native-inputs (list unzip))
    (home-page "https://github.com/v2fly/v2ray-core")
    (synopsis
     "A platform for building proxies to bypass network restrictions.")
    (description
     "Project V is a set of network tools that help you to build your
own computer network. It secures your network connections and thus protects your
privacy. See our website for more information.")
    (license license:expat)))

(define-public v2ray-sn
  (package
    (inherit v2ray)
    (name "v2ray-sn")
    (version "5.0.16")
    (source (origin
              (method url-fetch)
              ;; https://github.com/SagerNet/v2ray-core/releases/download/v5.0.16/v2ray-linux-64.zip
              (uri (string-append
                    "https://github.com/SagerNet/v2ray-core/releases/download/v"
                    version "/v2ray-linux-64.zip"))
              (sha256
               (base32
                "0f5aikgc2hnnx93wp9b1z0ms2rsyx9yyv1db4p7gjprn2dnkv3v6"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan '(("v2ray" "/bin/")
                        ("geoip.dat" "/bin/")
                        ("geosite.dat" "/bin/"))
       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key source #:allow-other-keys)
                      (invoke "unzip" source))))))
    (native-inputs (list unzip))
    (home-page "https://github.com/SagerNet/v2ray-core")
    (synopsis
     "A platform for building proxies to bypass network restrictions (for SagerNet :)")
    (description
     "Project V is a set of network tools that help you to build your
own computer network. It secures your network connections and thus protects your
privacy. See our website for more information.")
    (license license:expat)))

(define-public qv2ray
  (package
    (name "qv2ray")
    (version "fb44fb1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Qv2ray/Qv2ray")
                    ; (commit (string-append "v" version))
                    (commit "fb44fb1421941ab192229ff133bc28feeb4a8ce5")
                    (recursive? #t)))
              (file-name (git-file-name name version))
              ; git clone https://github.com/Qv2ray/Qv2ray ~/Downloads/Qv2ray
              ; guix hash --serializer=nar -x ~/Downloads/Qv2ray
              ; rm -rf ~/Downloads/Qv2ray
              (sha256
               (base32
                "1dg7i1488pdbq682nrqzbj8xy271ngb09sbm2q80mj6ann006y2f"))))
    (build-system qt-build-system)
    (arguments
     `(#:configure-flags '("-DQV2RAY_DISABLE_AUTO_UPDATE=ON"
                           "-DCMAKE_BUILD_TYPE=Release"
                           "-DQV2RAY_BUILD_INFO=Qv2ray for GNU Guix"
                           "-DQV2RAY_HAS_BUILTIN_THEMES=OFF"
                           ; "-DQV2RAY_QT6=ON"
                           "-DQV2RAY_USE_V5_CORE=ON")
       #:tests? #f))
    (native-inputs (list pkg-config
                         qttools-5
                         zlib
                         c-ares
                         abseil-cpp
                         re2
                         curl
                         grpc
                         protobuf
                         openssl
                         qtbase-5
                         qtsvg-5))
    (propagated-inputs (list v2ray-sn qtwayland-5))
    (home-page "https://github.com/Qv2ray/Qv2ray/")
    (synopsis
     "A cross-platform connection manager for V2Ray and other backends")
    (description "Binary version of Qv2ray")
    (license license:gpl3)))
