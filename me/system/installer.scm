;;; Copyright © 2023 Jiwei Yang <yangjiwei@protonmail.com>
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 David Wilson <david@daviwil.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Generate a bootable image (e.g. for USB sticks, etc.) with:
;; $ guix system image -t iso9660 installer.scm

(define-module (me system installer)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages nano)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages package-management)
  #:use-module (me packages linux)
  #:use-module (me utils kicksecure)
  #:use-module (nongnu packages linux)
  #:use-module (guix)
  #:export (installation-os-me))

(define installation-os-me
  (operating-system
    (inherit installation-os)
    (kernel linux-xanmod-hardened)
    (firmware (cons* ; iwlwifi-firmware
                     ; ibt-hw-firmware
                     realtek-firmware
                     %base-firmware))
    ;; Add the 'net.ifnames' argument to prevent network interfaces
    ;; from having really long names.  This can cause an issue with
    ;; wpa_supplicant when you try to connect to a wifi network.
    (kernel-arguments
      (append (cons*  "quiet" "modprobe.blacklist=radeon" "net.ifnames=0"

                      ;; for pci passthrough
                      "intel_iommu=on"
                      "amd_iommu=on"
                      ; "iommu=pt"
                      ; "vfio-pci.ids=8086:1901,10de:1f11,10de:10f9,10de:1ada,10de:1adb,1987:5008"

                      ;; CPU mitigations, we need SMT enabled because of performance
                      "mitigations=auto"

                      ;; harden
                      ; "module.sig_enforce=1" ; equivalent to CONFIG_MODULE_SIG_FORCE=y
                      "modprobe.blacklist=dccp,sctp,rds,tipc,n-hdlc,ax25,netrom,x25,rose,decnet,econet,af_802154,ipx,appletalk,psnap,p8023,p8022,can,atm,cramfs,freevxfs,jffs2,hfs,hfsplus,udf,nfs,nfsv3,nfsv4,ksmbd,gfs2,vivid,bluetooth,btusb,firewire-core,thunderbolt"
                      %kicksecure-kernel-arguments)
               %default-kernel-arguments))

    (services
     (cons*
      ;; Include the channel file so that it can be used during installation
      (simple-service 'channel-file etc-service-type
                      (list `("channels.scm" ,(local-file "channels.scm"))))
      (operating-system-user-services installation-os)))

    ;; Add some extra packages useful for the installation process
    (packages
     (append (list git curl stow nano)
             (operating-system-packages installation-os)))))

installation-os-me