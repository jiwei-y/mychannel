(list (channel
        (name 'mychannel)
        (url "https://github.com/jiwei-y/mychannel")
        (branch "master")
        (commit
          "4ee7570123a933f371a1c29f9dadf05b2332947d"))
      (channel
        (name 'guix-science)
        (url "https://github.com/guix-science/guix-science.git")
        (branch "master")
        (commit
          "5fc5e3e855c298a692c62129e9edd0fcc7c6949f")
        (introduction
          (make-channel-introduction
            "b1fe5aaff3ab48e798a4cce02f0212bc91f423dc"
            (openpgp-fingerprint
              "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446"))))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "4094f7ae475a4f767fb407520ed1cc8c699ff29e")
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (branch "master")
        (commit
          "67d2f688fb89553df53e73a4c584b1b9eb7d5c24")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
