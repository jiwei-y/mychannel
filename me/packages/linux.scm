(define-module (me packages linux)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (me packages)
  #:use-module (me utils download)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex))

(define %upstream-linux-source
  (@@ (gnu packages linux) %upstream-linux-source))

(define source-with-patches
  (@@ (gnu packages linux) source-with-patches))

(define %default-extra-linux-options
  (@@ (gnu packages linux) %default-extra-linux-options))

(define config->string
  (@@ (gnu packages linux) config->string))

(define-public upstream-version "6.1.11")
(define-public upstream-major-version
  (version-major+minor upstream-version))
(define-public xanmod-hardened-version  "6.1.11")
(define-public xanmod-version "6.1.11")
(define-public xanmod-revision "xanmod1")
(define-public hardened-version "6.1.11")
(define-public hardened-revision "hardened1")

(define-public linux-pristine-source
  (let ((version upstream-major-version)
        ;; mirror://kernel.org/linux/kernel/v6.x/linux-6.1.tar.xz
        ;; https://cdn.kernel.org/pub/linux/kernel/v6.x/linux-6.1.tar.xz
        (hash (base32 "1ssxn81jfl0jf9brczfrrwd1f1vmf594jvhrs7zgcc54a5qg389c")))
    (%upstream-linux-source version hash)))

(define %xanmod-patch
  (origin
    (method url-fetch/xz-file)
    ;; guix download https://github.com/xanmod/linux/releases/download/6.1.11-xanmod1/patch-6.1.11-xanmod1.xz -o ~/Downloads/6.1.11-xanmod1.xz 
    (file-name (string-append "linux-" xanmod-version "-" xanmod-revision ".patch"))
    (uri (string-append "https://github.com/xanmod/linux"
                        "/releases/download/" xanmod-version "-" xanmod-revision
                        "/patch-" xanmod-version "-" xanmod-revision ".xz"))
    (sha256 (base32
             "0qb6cmf6pjp4k4yk1nbsd9klnynharhyma2lzpnk9imm6mx3dqap"))))

(define %hardened-patch
  (origin
    (method url-fetch)
    ;; guix download https://github.com/anthraxx/linux-hardened/releases/download/6.1.11-hardened1/linux-hardened-6.1.11-hardened1.patch -o ~/dotfiles/mychannel/me/packages/patches/linux-6.1.11-hardened1.patch 
    (file-name (string-append "linux-" hardened-version "-" hardened-revision ".patch"))
    (uri (string-append
          "https://github.com/anthraxx/linux-hardened/releases/download/"
          hardened-version "-" hardened-revision "/linux-hardened-" hardened-version "-" hardened-revision ".patch"))
    (sha256 (base32
             "1pydcjy2cjnb4zxcqr41hr34fg8alph314xasdsfvdw4zaz55s6h"))))

; (define %adjusted-hardened-patch
;   (let* ((version hardened-version)
;          (patch (string-append "linux-" version ".patch"))
;          (source (origin
;                    (method url-fetch)
;                    ;; guix download https://github.com/anthraxx/linux-hardened/releases/download/6.1.11-hardened1/linux-hardened-6.1.11-hardened1.patch -o ~/Downloads/linux-6.1.11-hardened1.patch
;                    (uri (string-append
;                          "https://github.com/anthraxx/linux-hardened/releases/download/"
;                          version "/linux-hardened-" version ".patch"))
;                    (sha256 (base32
;                             "12si2gy6maxbvf252ircp94ci0ihqlxv3l9sf4xwxrs66gn3z2fa")))))
;     (origin
;       (method computed-origin-method)
;       (file-name patch)
;       (sha256 #f)
;       (uri
;         (delay
;           (with-imported-modules '((guix build utils))
;             #~(begin
;                 (use-modules (guix build utils)
;                              (srfi srfi-1)
;                              (ice-9 match)
;                              (ice-9 ftw))
;                 (copy-file #+source #$patch)
;                 (make-file-writable #$patch)
;                 (chmod #$patch #o755)
;                 (substitute* #$patch
;                   (("SUBLEVEL = 15") "SUBLEVEL = 0")
;                   (("EXTRAVERSION = -hardened1") "EXTRAVERSION ="))
;                 (copy-file #$patch
;                            #$output))))))))

(define %vfio-pci-pm-patch
  (origin
    (method url-fetch)
    (uri (string-append "https://patchwork.kernel.org/series/671981/mbox/"))
    (file-name "vfio-pci-power-management-changes.patch")
    (sha256 (base32
             "07agmxqa338wqmf3gxf90hn1384wc621mmhl7m8xj9bliq0lqj83"))))

(define-public xanmod-hardened-source
  (origin
    (inherit (source-with-patches
              linux-pristine-source
              (list ;; %vfio-pci-pm-patch
                    %xanmod-patch
                    ;; %hardened-patch
                    ; find " .procname	= "unprivileged_userns_clone", ", delete that trunk
                    (local-file "patches/linux-6.1.11-hardened1.patch"))))
    (modules '((guix build utils)))))

;(define-public xanmod-source
;  (origin
;    (inherit (source-with-patches
;              linux-pristine-source
;              (list ;; %vfio-pci-pm-patch
;                    %xanmod-patch)))
;    (modules '((guix build utils)))))
;
;(define-public hardened-source
;  (origin
;    (inherit (source-with-patches
;              linux-pristine-source
;              (list ;; %vfio-pci-pm-patch
;                    %hardened-patch)))
;    (modules '((guix build utils)))))

(define %waydroid-extra-linux-options
  `( ;Modules required for waydroid:
      ("CONFIG_ASHMEM" . #t)
      ("CONFIG_ANDROID" . #t)
      ("CONFIG_ANDROID_BINDER_IPC" . #t)
      ("CONFIG_ANDROID_BINDERFS" . #t)
      ("CONFIG_ANDROID_BINDER_DEVICES" . "binder,hwbinder,vndbinder")))

(define %khc-extra-linux-options
  `(  ;; kconfig-hardened-check (remember to apply the configs in the comments)
      ;; wget -O ~/dotfiles/mychannel/me/packages/aux-files/config_x86-64-v2 https://github.com/xanmod/linux/raw/6.1/CONFIGS/xanmod/gcc/config_x86-64-v2
      ;; kconfig-hardened-check -m show_fail -c ~/dotfiles/mychannel/me/packages/aux-files/config_x86-64-v2
      ;? CONFIG_LOCK_DOWN_KERNEL_FORCE_CONFIDENTIALITY is not set

      ;; dependencies of hardened config (UBSAN)
      ("CONFIG_CC_HAS_UBSAN_BOUNDS" . #t)
      ("CONFIG_CC_HAS_UBSAN_ARRAY_BOUNDS" . #t)

      ("CONFIG_GCC_PLUGINS" . #t)
      ("CONFIG_WERROR" . #t)
      ("CONFIG_BUG_ON_DATA_CORRUPTION" . #t)
      ("CONFIG_DEBUG_LIST" . #t)
      ("CONFIG_DEBUG_VIRTUAL" . #t)
      ("CONFIG_DEBUG_SG" . #t)
      ("CONFIG_DEBUG_CREDENTIALS" . #t)
      ("CONFIG_DEBUG_NOTIFIERS" . #t)
      ("CONFIG_ZERO_CALL_USED_REGS" . #t)
      ("CONFIG_STATIC_USERMODEHELPER" . #t)
      ("CONFIG_RANDSTRUCT_FULL" . #t)
      ("CONFIG_RANDSTRUCT_PERFORMANCE" . #f)
      ("CONFIG_GCC_PLUGIN_LATENT_ENTROPY" . #t)
      ("CONFIG_MODULE_SIG_FORCE" . #t)
      ("CONFIG_INIT_STACK_ALL_ZERO" . #t)
      ("CONFIG_INIT_ON_FREE_DEFAULT_ON" . #t)
      ("CONFIG_EFI_DISABLE_PCI_DMA" . #t)
      ("CONFIG_UBSAN_BOUNDS" . #t)
      ("CONFIG_UBSAN_LOCAL_BOUNDS" . #t)
      ("CONFIG_UBSAN_TRAP" . #t)
      ("CONFIG_UBSAN_SANITIZE_ALL" . #t)
      ("CONFIG_GCC_PLUGIN_STACKLEAK" . #t)
      ("CONFIG_STACKLEAK_METRICS" . #f)
      ("CONFIG_STACKLEAK_RUNTIME_DISABLE" . #f)
      ("CONFIG_CFI_CLANG" . #t)
      ("CONFIG_CFI_PERMISSIVE" . #f)
      ("CONFIG_IOMMU_DEFAULT_DMA_STRICT" . #t)
      ("CONFIG_INTEL_IOMMU_DEFAULT_ON" . #t)
      ("CONFIG_AMD_IOMMU_V2" . #t)
      ("CONFIG_SLAB_MERGE_DEFAULT" . #f)
      ("CONFIG_SECURITY_SELINUX_BOOTPARAM" . #f)
      ("CONFIG_SECURITY_SELINUX_DEVELOP" . #f)
      ; ("CONFIG_LOCK_DOWN_KERNEL_FORCE_CONFIDENTIALITY" . #t)
      ("CONFIG_BINFMT_MISC" . #f)
      ("CONFIG_INET_DIAG" . #f)
      ("CONFIG_KEXEC" . #f)
      ("CONFIG_PROC_KCORE" . #f)
      ("CONFIG_LEGACY_PTYS" . #f)
      ("CONFIG_HIBERNATION" . #f)
      ("CONFIG_COMPAT" . #f)
      ("CONFIG_IA32_EMULATION" . #f)
      ("CONFIG_MODIFY_LDT_SYSCALL" . #f)
      ("CONFIG_X86_MSR" . #f)
      ("CONFIG_MODULES" . #f)
      ("CONFIG_DEVMEM" . #f)
      ("CONFIG_IO_STRICT_DEVMEM" . #t)
      ("CONFIG_LDISC_AUTOLOAD" . #f)
      ("CONFIG_LEGACY_VSYSCALL_NONE" . #t)
      ("CONFIG_PROC_VMCORE" . #f)
      ("CONFIG_PROC_PAGE_MONITOR" . #f)
      ("CONFIG_USELIB" . #f)
      ("CONFIG_CHECKPOINT_RESTORE" . #f)
      ("CONFIG_USERFAULTFD" . #f)
      ("CONFIG_HWPOISON_INJECT" . #f)
      ("CONFIG_MEM_SOFT_DIRTY" . #f)
      ("CONFIG_DEVPORT" . #f)
      ("CONFIG_DEBUG_FS" . #f)
      ("CONFIG_NOTIFIER_ERROR_INJECTION" . #f)
      ("CONFIG_PUNIT_ATOM_DEBUG" . #f)
      ("CONFIG_ACPI_CONFIGFS" . #f)
      ("CONFIG_MTD_SLRAM" . #f)
      ("CONFIG_MTD_PHRAM" . #f)
      ("CONFIG_IO_URING" . #f)
      ("CONFIG_KCMP" . #f)
      ("CONFIG_RSEQ" . #f)
      ("CONFIG_SUNRPC_DEBUG" . #f)
      ("CONFIG_FB" . #f)
      ("CONFIG_VT" . #f)
      ("CONFIG_BLK_DEV_FD" . #f)
      ("CONFIG_STAGING" . #f)
      ("CONFIG_KSM" . #f)
      ("CONFIG_KALLSYMS" . #f)
      ("CONFIG_X86_VSYSCALL_EMULATION" . #f)
      ("CONFIG_MAGIC_SYSRQ" . #f)
      ("CONFIG_KEXEC_FILE" . #f)
      ("CONFIG_USER_NS" . #f)
      ("CONFIG_X86_CPUID" . #f)
      ("CONFIG_X86_IOPL_IOPERM" . #f)
      ("CONFIG_ACPI_TABLE_UPGRADE" . #f)
      ("CONFIG_EFI_CUSTOM_SSDT_OVERLAYS" . #f)
      ("CONFIG_COREDUMP" . #f)
      ("CONFIG_EFI_TEST" . #f)
      ("CONFIG_KPROBES" . #f)
      ("CONFIG_BPF_SYSCALL" . #f)
      ("CONFIG_IP_DCCP" . #f)
      ("CONFIG_IP_SCTP" . #f)
      ("CONFIG_VIDEO_VIVID" . #f)
      ("CONFIG_KGDB" . #f)
      ("CONFIG_TRIM_UNUSED_KSYMS" . #t)))
  
(define %personal-extra-options
  `(  ;; kheaders module to avoid building failure
      ; ("CONFIG_IKHEADERS" . #f)
      ;; modules required for initird
      ("CONFIG_CRYPTO_XTS" . m)
      ("CONFIG_VIRTIO_CONSOLE" . m)
      ;; built in VFIO modules for pci passthrough
      ; ("CONFIG_VFIO_PCI" . #t)
      ; ("CONFIG_VFIO_VIRQFD" . #t)
      ;; modprobe path on guix
      ("CONFIG_MODPROBE_PATH" . "/run/current-system/profile/bin/modprobe")

      ;; adjustment to khc
      ("CONFIG_MODULES" . #t)
      ("CONFIG_FB" . #t)
      ("CONFIG_VT" . #t)
      ; CONFIG_STATIC_USERMODEHELPER is not set 
      ; CONFIG_SECURITY_LOADPIN is not set 
      ; CONFIG_SECURITY_LOADPIN_ENFORCE is not set 
      ;? CONFIG_WERROR is not set 
      ;? CONFIG_LOCK_DOWN_KERNEL_FORCE_CONFIDENTIALITY is not set
      ;? CONFIG_MODULE_SIG_FORCE is not set
      ("CONFIG_STATIC_USERMODEHELPER" . #f)
      ("CONFIG_SECURITY_LOADPIN" . #f)
      ("CONFIG_SECURITY_LOADPIN_ENFORCE" . #f)
      ("CONFIG_WERROR" . #f)
      ("CONFIG_LOCK_DOWN_KERNEL_FORCE_CONFIDENTIALITY" . #f)
      ("CONFIG_MODULE_SIG_FORCE" . #f)
      ("CONFIG_HIBERNATION" . #t)
      ; LKRG requires CONFIG_KPROBES
      ; LKRG requires CONFIG_KALLSYMS_ALL (depends on CONFIG_KALLSYMS)
      ; LKRG requires ("CONFIG_TRIM_UNUSED_KSYMS" . #f) if it should be built as an out-of-tree kernel module
      ; LKRG requires CONFIG_SECURITY_SELINUX_BOOTPARAM
      ; LKRG requires CONFIG_SECURITY_SELINUX_DEVELOP
      ("CONFIG_KALLSYMS" . #t)
      ("CONFIG_KPROBES" . #t)
      ("CONFIG_TRIM_UNUSED_KSYMS" . #f)
      ("CONFIG_SECURITY_SELINUX_BOOTPARAM" . #t)
      ("CONFIG_SECURITY_SELINUX_DEVELOP" . #t)
      ;; LKRG in-tree module (not supported yet)
      ; ("CONFIG_SECURITY_LKRG" . #t)

      ;; use custom DSDT to enable s3 sleep
      ("CONFIG_ACPI_CUSTOM_DSDT" . #t)
      ("CONFIG_ACPI_CUSTOM_DSDT_FILE" . "dsdt.hex")

      ;; cpu specified optimisation
      ("CONFIG_GENERIC_CPU" . #f)
      ("CONFIG_GENERIC_CPU2" . #f)
      ("CONFIG_MZEN3" . #f)
      ("CONFIG_MNATIVE_INTEL" . #f)
      ("CONFIG_MNATIVE_AMD" . #t)))

;(define* (kernel-config arch #:key variant)
;  "Return a file-like object of the Linux-Libre build configuration file for
;ARCH and optionally VARIANT, or #f if there is no such configuration."
;  (let* ((file "config_x86-64-v2"))
;    (local-file (search-me-auxiliary-file file))))


;(define* (corrupt-linux version
;                        revision
;                        source
;                        supported-systems
;                        #:key (extra-version #f)
;                        ;; A function that takes an arch and a variant.
;                        ;; See kernel-config for an example.
;                        (configuration-file #f)
;                        (defconfig "defconfig")
;                        (extra-options %default-extra-linux-options))
;  ((@@ (gnu packages linux) make-linux-libre*)
;   version
;   revision
;   source
;   supported-systems
;   #:extra-version extra-version
;   #:configuration-file configuration-file
;   #:defconfig defconfig
;   #:extra-options extra-options))
;
;(define* (corrupt-linux-headers version
;                               revision
;                               source)
;  ((@@ (gnu packages linux) make-linux-libre-headers*)
;   version
;   revision
;   source))

(define-public linux-xanmod-hardened
  (let ((base (customize-linux #:name "linux-xanmod-hardened"
                               #:source xanmod-hardened-source
                               #:defconfig "config_x86-64-v2"
                               ;; Extraversion is used instead.
                               #:configs (config->string
                                          '(("CONFIG_LOCALVERSION" . "")))
                               #:extra-version xanmod-revision)))
    (package
      (inherit base)
      (version xanmod-hardened-version)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags flags)
           `(append '("CFLAGS=-O3")
                   ,flags))
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'remove-localversion
                (lambda _
                  (when (file-exists? "localversion")
                    (delete-file "localversion"))))
              (add-after 'remove-localversion 'customize-dsdt
                (lambda* (#:key copy-file #:allow-other-keys)
                  (copy-file (local-file "DSDT/dsdt.hex") "dsdt.hex")
                  (chmod ".config" #o444)))
              (add-after 'patch-source-shebangs 'patch-randstruct
              ;; customize the kernel RANDSTRUCT seed
                (lambda* (#:key inputs target #:allow-other-keys)
                          (substitute* "scripts/gen-randstruct-seed.sh"
                            (("od -A n -t x8 -N 32 /dev/urandom") 
                              "echo $ARCH $EXTRAVERSION $KBUILD_BUILD_USER $PATH $C_INCLUDE_PATH $CPLUS_INCLUDE_PATH | sha256sum | cut -d ' ' -f 1")
                            (("tr -d ' ") 
                              "tr -d '"))))
              (add-before 'configure 'add-defconfig
                (lambda _
                  (copy-file "CONFIGS/xanmod/gcc/config_x86-64-v2" ".config")
                  ;; Adapted from `make-linux-libre*'.
                  (chmod ".config" #o666)
                  (let ((port (open-file ".config" "a"))
                        (extra-configuration #$(config->string
                                                ;; FIXME: There might be other
                                                ;; support missing.
                                                (append ; %waydroid-extra-linux-options
                                                        %khc-extra-linux-options
                                                        %personal-extra-options
                                                        %default-extra-linux-options))))
                    (display extra-configuration port)
                    (close-port port))
                  (invoke "make" "oldconfig")
                  (rename-file ".config" "arch/x86/configs/config_x86-64-v2")))
              (add-after 'configure 'harden-config
              ;; do some harden which we can't do in extra options
                (lambda* (#:key inputs #:allow-other-keys)
                  (substitute* '(".config" "arch/x86/configs/guix_defconfig")
                    (("CONFIG_ARCH_MMAP_RND_BITS=28") 
                    "CONFIG_ARCH_MMAP_RND_BITS=32"))))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         ;; cpio is needed for CONFIG_IKHEADERS.
         (append gcc-12 cpio zstd)))
      (home-page "https://github.com/anthraxx/linux-hardened")
      (supported-systems '("x86_64-linux"))
      (synopsis
       "Xanmod + hardened")
      (description
       "Linux kernel with Xanmod and hardened patches"))))

;(define (make-xanmod-hardened version)
;  (let ((base-xanmod-hardened
;          (corrupt-linux version
;                         ""
;                         xanmod-hardened-source
;                         '("x86_64-linux" "i686-linux")
;                         #:defconfig "config_x86-64-v2"
;                         #:extra-options (append
;                                          ; %waydroid-extra-linux-options
;                                          %khc-extra-linux-options
;                                          %personal-extra-options
;                                          %default-extra-linux-options))))
;    (package
;      (inherit base-xanmod-hardened)
;      (name "linux-xanmod-hardened")
;      (version version)
;      (arguments
;        (substitute-keyword-arguments (package-arguments base-xanmod-hardened)
;          ((#:configure-flags flags)
;            `(append '("CFLAGS=-O3")
;                    ,flags))
;          ((#:phases phases)
;            #~(modify-phases #$phases
;                (add-after 'patch-source-shebangs 'patch-randstruct
;                ;; customize the kernel RANDSTRUCT seed
;                  (lambda* (#:key inputs target #:allow-other-keys)
;                            (substitute* "scripts/gen-randstruct-seed.sh"
;                              (("od -A n -t x8 -N 32 /dev/urandom") 
;                                "echo $ARCH $EXTRAVERSION $KBUILD_BUILD_USER $PATH $C_INCLUDE_PATH $CPLUS_INCLUDE_PATH | sha256sum | cut -d ' ' -f 1")
;                              (("tr -d ' ") 
;                                "tr -d '"))))
;                (add-after 'configure 'harden-config
;                ;; do some harden which we can't do in extra options
;                  (lambda* (#:key inputs #:allow-other-keys)
;                    (substitute* ".config"
;                      (("CONFIG_ARCH_MMAP_RND_BITS=28") 
;                      "CONFIG_ARCH_MMAP_RND_BITS=32"))))))))
;      (native-inputs (modify-inputs (package-native-inputs linux-libre)
;                      (append gcc-12 cpio zstd)))
;    (home-page "https://github.com/anthraxx/linux-hardened")
;    (synopsis "Xanmod + hardened")
;    (description
;     "Linux kernel with Xanmod and hardened patches"))))
;
;(define (make-linux-xanmod version)
;  (let ((base-linux-xanmod
;          (corrupt-linux version
;                         ""
;                         xanmod-source
;                         '("x86_64-linux" "i686-linux")
;                         #:configuration-file kernel-config
;                         #:extra-options (append
;                                          ; %waydroid-extra-linux-options
;                                          ; %khc-extra-linux-options
;                                          %personal-extra-options
;                                          %default-extra-linux-options))))
;    (package
;      (inherit base-linux-xanmod)
;      (name "linux-xanmod")
;      (version version)
;      (arguments
;        (substitute-keyword-arguments (package-arguments base-linux-xanmod)
;          ((#:configure-flags flags)
;            `(append '("CFLAGS=-O3")
;                    ,flags))
;          ((#:phases phases)
;            #~(modify-phases #$phases
;                (add-after 'patch-source-shebangs 'patch-randstruct
;                ;; customize the kernel RANDSTRUCT seed
;                  (lambda* (#:key inputs target #:allow-other-keys)
;                            (substitute* "scripts/gen-randstruct-seed.sh"
;                              (("od -A n -t x8 -N 32 /dev/urandom") 
;                                "echo $ARCH $EXTRAVERSION $KBUILD_BUILD_USER $PATH $C_INCLUDE_PATH $CPLUS_INCLUDE_PATH | sha256sum | cut -d ' ' -f 1")
;                              (("tr -d ' ") 
;                                "tr -d '"))))
;                (add-after 'configure 'harden-config
;                ;; do some harden which we can't do in extra options
;                  (lambda* (#:key inputs #:allow-other-keys)
;                    (substitute* ".config"
;                      (("CONFIG_ARCH_MMAP_RND_BITS=28") 
;                      "CONFIG_ARCH_MMAP_RND_BITS=32"))))))))
;      (native-inputs (modify-inputs (package-native-inputs linux-libre)
;                      (append gcc-12 cpio zstd)))
;      (home-page "https://xanmod.org/")
;      (synopsis "The Linux kernel and modules with Xanmod patches")
;      (description
;      "XanMod is a general-purpose Linux kernel distribution with custom settings and new features.
;    Built to provide a stable, responsive and smooth desktop experience."))))
;
;(define (make-linux-xanmod-headers version)
;  (package
;    (inherit (corrupt-linux-headers version
;                                   ""
;                                   xanmod-source))
;    (name "linux-xanmod-headers")
;    (version version)
;    (native-inputs (modify-inputs (package-native-inputs linux-libre-headers)
;                     (append gcc-12)))
;    (home-page "https://xanmod.org/")
;    (synopsis "Linux-Xanmod kernel headers")
;    (description "Headers of the Linux-Xanmod kernel.")))
;
;(define (make-linux-hardened version)
;  (let ((base-linux-hardened
;          (corrupt-linux version
;                         ""
;                         hardened-source
;                         '("x86_64-linux" "i686-linux")
;                         ;#:configuration-file kernel-config
;                         #:extra-options (append
;                                          ; %waydroid-extra-linux-options
;                                          %khc-extra-linux-options
;                                          %personal-extra-options
;                                          %default-extra-linux-options))))
;    (package
;      (inherit base-linux-hardened)
;      (name "linux-hardened")
;      (version version)
;      (arguments
;        (substitute-keyword-arguments (package-arguments base-linux-hardened)
;          ((#:configure-flags flags)
;            `(append '("CFLAGS=-O3")
;                    ,flags))
;          ((#:phases phases)
;            #~(modify-phases #$phases
;                (add-after 'patch-source-shebangs 'patch-randstruct
;                ;; customize the kernel RANDSTRUCT seed
;                  (lambda* (#:key inputs target #:allow-other-keys)
;                            (substitute* "scripts/gen-randstruct-seed.sh"
;                              (("od -A n -t x8 -N 32 /dev/urandom") 
;                                "echo $ARCH $EXTRAVERSION $KBUILD_BUILD_USER $PATH $C_INCLUDE_PATH $CPLUS_INCLUDE_PATH | sha256sum | cut -d ' ' -f 1")
;                              (("tr -d ' ") 
;                                "tr -d '"))))
;                (add-after 'configure 'harden-config
;                ;; do some harden which we can't do in extra options
;                  (lambda* (#:key inputs #:allow-other-keys)
;                    (substitute* ".config"
;                      (("CONFIG_ARCH_MMAP_RND_BITS=28") 
;                      "CONFIG_ARCH_MMAP_RND_BITS=32"))))))))
;      (native-inputs (modify-inputs (package-native-inputs linux-libre)
;                      (append gcc-12 cpio zstd)))
;    (home-page "https://github.com/anthraxx/linux-hardened")
;    (synopsis "Minimal supplement to upstream Kernel Self Protection Project changes")
;    (description
;     "Minimal supplement to upstream Kernel Self Protection Project changes. Features already provided by SELinux + Yama and archs other than multiarch arm64 / x86_64 aren't in scope."))))
;
;(define (make-linux-hardened-headers version)
;  (package
;    (inherit (corrupt-linux-headers version
;                                   ""
;                                   hardened-source))
;    (name "linux-hardened-headers")
;    (version version)
;    (native-inputs (modify-inputs (package-native-inputs linux-libre-headers)
;                     (append gcc-12)))
;    (home-page "https://github.com/anthraxx/linux-hardened")
;    (synopsis "Linux-hardened kernel headers")
;    (description "Headers of the Linux-hardened kernel.")))
;
;(define-public linux-xanmod-hardened
;  (make-xanmod-hardened xanmod-hardened-version))
;
;(define-public linux-xanmod
;  (make-linux-xanmod xanmod-version))
;
;(define-public linux-hardened
;  (make-linux-hardened hardened-version))

(define-public lkrg-my
  (package 
    (inherit lkrg)
    (name "lkrg-my")
    (version "0.9.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lkrg-org/lkrg")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               ; git clone -b v0.9.6 --depth 1 https://github.com/lkrg-org/lkrg ~/Downloads/lkrg
               ; guix hash --serializer=nar -x ~/Downloads/lkrg
               ; rm -rf ~/Downloads/lkrg
               (base32
                "0sm9vp33kwhhp4h6j2k01mijk0hwyx4xvdysj3w3g87xh194v9mj"))))
    (arguments
        (substitute-keyword-arguments (package-arguments lkrg)
          ((#:linux linux) linux-xanmod-hardened)
          ((#:phases phases '%standard-phases)
             `(modify-phases ,phases
                (add-after 'unpack 'patch-source
                  (lambda* (#:key inputs #:allow-other-keys)
                    (substitute* "src/modules/exploit_detection/syscalls/p_call_usermodehelper/p_call_usermodehelper.c"
                      ;; Use path to /gnu/store/*-kmod in actual path that is
                      ;; exec'ed.
                      (("\"/sbin/modprobe\"")
                      (string-append "\""
                                      (search-input-file inputs "/bin/modprobe")
                                      "\""))
                      ; (("/bin/true") (search-input-file inputs "/bin/true"))
                      ; (("/bin/false") (search-input-file inputs "/bin/false"))
                    )))))))
    (inputs (modify-inputs (package-inputs lkrg)
              (prepend coreutils
                       kmod)))))

(define-public kconfig-hardened-check
  (package
    (name "kconfig-hardened-check")
    (version "20230122")
    (source
     ;; The PyPI tarball does not contain the tests.
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/a13xp0p0v/kconfig-hardened-check")
             ; https://github.com/a13xp0p0v/kconfig-hardened-check/commits/master
             (commit "295a293b0f21b016b1a9ec0eae1f29e52e70cff1")))
       (file-name (git-file-name name version))
       (sha256
        ; git clone --depth 1 https://github.com/a13xp0p0v/kconfig-hardened-check ~/Downloads/kconfig-hardened-check
        ; guix hash --serializer=nar -x ~/Downloads/kconfig-hardened-check
        ; rm -rf ~/Downloads/kconfig-hardened-check
        (base32 "0bpdy2a7l75y5cqzzc92nh4gapzgza8ml5i8ximr6brf6pr3681z"))))
    (build-system python-build-system)
    (arguments
     '( #:tests? #f
        #:phases
          (modify-phases %standard-phases
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke "pytest")))))))
    (home-page "https://github.com/a13xp0p0v/kconfig-hardened-check")
    (synopsis "A tool for checking the security hardening options of the Linux kernel ")
    (description
     "kconfig-hardened-check.py helps me to check the Linux kernel options against my security hardening preferences, which are based on the

      KSPP recommended settings,
      CLIP OS kernel configuration,
      Last public grsecurity patch (options which they disable),
      SECURITY_LOCKDOWN_LSM patchset,
      Direct feedback from Linux kernel maintainers (see #38, #53, #54, #62).

      This tool supports checking Kconfig options and kernel cmdline parameters.")
    (license gpl3)))

(define-public python-kconfiglib
  (package
    (name "python-kconfiglib")
    (version "14.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "kconfiglib" version))
              (sha256
               (base32
                "0g690bk789hsry34y4ahvly5c8w8imca90ss4njfqf7m2qicrlmy"))))
    (build-system python-build-system)
    (home-page "https://github.com/ulfalizer/Kconfiglib")
    (synopsis "A flexible Python Kconfig implementation")
    (description
     "This package provides a flexible Python Kconfig implementation")
    (license #f)))

(define-public tuxedo-keyboard
  (package
    (name "tuxedo-keyboard")
    (version "3.1.1")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/tuxedocomputers/tuxedo-keyboard")
                (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               ; git clone -b v3.1.1 --depth 1 https://github.com/tuxedocomputers/tuxedo-keyboard ~/Downloads/tuxedo-keyboard
               ; guix hash --serializer=nar -x ~/Downloads/tuxedo-keyboard
               ; rm -rf ~/Downloads/tuxedo-keyboard
               (base32
                "17n14yh55yrxx4qbx4ph9drbzx2ll4kdsfmlngrdgizhyzk7z7zv"))))
    (build-system linux-module-build-system)
    (arguments
     (list #:linux linux-xanmod-hardened
           #:tests? #f
;            #:phases
;              #~(modify-phases %standard-phases
;                 (add-after 'unpack 'patch-randstruct
;                 ;; customize the kernel RANDSTRUCT seed
;                   (lambda* (#:key inputs target #:allow-other-keys)
;                     (let ((config (search-input-file linux-module-builder "lib/modules/build/CONFIGS/xanmod/gcc/config_x86-64-v2")))
;                             (setenv "RANDSTRUCT_CFG" config)
;                             (substitute* (search-input-file linux-module-builder "gen-randstruct-seed.sh")
;                               (("od -A n -t x8 -N 32 /dev/urandom") 
;                               "echo $ARCH $EXTRAVERSION $RANDSTRUCT_CFG $PATH $C_INCLUDE_PATH $CPLUS_INCLUDE_PATH | sha256sum | cut -d ' ' -f 1"))))))
))
    (home-page "https://github.com/tuxedocomputers/tuxedo-keyboard")
    (synopsis "Tuxedo computers kernel module for keyboard
backlighting")
    (description "Tuxedo computer kernel module for keyboard
backlighting.")
    (license gpl2)))