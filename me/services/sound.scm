
(define-module (me services sound)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)

  #:export (home-pipewire-services
            system-pipewire-services))

(define* (home-pipewire-services
        #:key
        (pipewire pipewire)
        (wireplumber wireplumber)
        (pulseaudio pulseaudio))
    (list
     ;; TODO: Make home-alsa-service-type
        (simple-service
        'pipewire-add-asoundrc
        home-xdg-configuration-files-service-type
        `(("alsa/asoundrc"
            ,(mixed-text-file
            "asoundrc"
            #~(string-append
                "<"
            #$(file-append
                    pipewire "/share/alsa/alsa.conf.d/50-pipewire.conf")
            ">\n<"
            #$(file-append
                    pipewire "/share/alsa/alsa.conf.d/99-pipewire-default.conf")
                ">\n"
                "
    pcm_type.pipewire {
    lib " #$(file-append pipewire "/lib/alsa-lib/libasound_module_pcm_pipewire.so")
    "
    }

    ctl_type.pipewire {
    lib " #$(file-append pipewire "/lib/alsa-lib/libasound_module_ctl_pipewire.so")
    "
    }
    ")))))
        (simple-service
        'pipewire-add-shepherd-daemons
        home-shepherd-service-type
        (list
        (shepherd-service
            (requirement '(dbus))
            (provision '(pipewire))
            (stop  #~(make-kill-destructor))
            (start #~(make-forkexec-constructor
                    (list #$(file-append pipewire "/bin/pipewire"))
                    #:log-file (string-append
                                (or (getenv "XDG_LOG_HOME")
                                    (format #f "~a/.local/var/log"
                                            (getenv "HOME")))
                                "/pipewire.log")
                    #:environment-variables
                    (append (list "DISABLE_RTKIT=1")
                            (default-environment-variables)))))
        (shepherd-service
            (requirement '(pipewire))
            (provision '(wireplumber))
            (stop  #~(make-kill-destructor))
            (start #~(make-forkexec-constructor
                    (list #$(file-append wireplumber "/bin/wireplumber"))
                    #:log-file (string-append
                                (or (getenv "XDG_LOG_HOME")
                                    (format #f "~a/.local/var/log"
                                            (getenv "HOME")))
                                "/wireplumber.log")
                    #:environment-variables
                    (append (list "DISABLE_RTKIT=1")
                            (default-environment-variables)))))
        (shepherd-service
            (requirement '(pipewire))
            (provision '(pipewire-pulse))
            (stop  #~(make-kill-destructor))
            (start #~(make-forkexec-constructor
                    (list #$(file-append pipewire "/bin/pipewire-pulse"))
                    #:log-file (string-append
                                (or (getenv "XDG_LOG_HOME")
                                    (format #f "~a/.local/var/log"
                                            (getenv "HOME")))
                                "/pipewire-pulse.log")
                    #:environment-variables
                    (append (list "DISABLE_RTKIT=1")
                            (default-environment-variables)))))))

        (simple-service
        'pipewire-add-packages
        home-profile-service-type
        (list pipewire wireplumber))))

  (define* (system-pipewire-services
        #:key
        (pipewire pipewire)
        (wireplumber wireplumber)
        (pulseaudio pulseaudio))
    (list
     (udev-rules-service
      'pipewire-add-udev-rules
      pipewire)))
