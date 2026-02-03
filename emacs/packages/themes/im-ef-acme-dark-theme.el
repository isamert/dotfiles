;;; im-ef-acme-dark-theme.el --- Legible dark theme with Acme/Plan 9 colors -*- lexical-binding:t -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Generated
;; Maintainer: Generated
;; Keywords: faces, theme, accessibility

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A dark theme combining the ef-themes framework with colors inspired
;; by the classic Acme/Plan 9 palette, adapted for dark backgrounds.

;; AI generated.

;;; Code:

(require 'ef-themes)

(defconst im-ef-acme-dark-palette-partial
  '((cursor "#7EB8DA")
    (bg-main "#1C1C18")
    (bg-dim "#252520")
    (bg-alt "#2E2E28")
    (fg-main "#E8E8D0")
    (fg-dim "#A0A090")
    (fg-alt "#B8B0E0")
    (bg-active "#4A4A40")
    (bg-inactive "#222220")
    (border "#5A5A50")

    ;; Core colors - lightened versions of Acme palette for dark bg
    (red "#E07070")
    (red-warmer "#F08060")
    (red-cooler "#E080A0")
    (red-faint "#C09090")
    (green "#80C080")
    (green-warmer "#90B870")
    (green-cooler "#70C0A0")
    (green-faint "#90A890")
    (yellow "#D0D080")
    (yellow-warmer "#E0C070")
    (yellow-cooler "#C8C090")
    (yellow-faint "#B0B080")
    (blue "#7EB8DA")
    (blue-warmer "#90A0E0")
    (blue-cooler "#70B0F0")
    (blue-faint "#A0B0C8")
    (magenta "#B0A0D0")
    (magenta-warmer "#C090C0")
    (magenta-cooler "#A0A0E0")
    (magenta-faint "#A898B8")
    (cyan "#70C0C0")
    (cyan-warmer "#80B0A8")
    (cyan-cooler "#60C0D0")
    (cyan-faint "#90B0B0")

    ;; Intense backgrounds for search, etc.
    (bg-red-intense "#5A2828")
    (bg-green-intense "#285028")
    (bg-yellow-intense "#4A4828")
    (bg-blue-intense "#283850")
    (bg-magenta-intense "#402850")
    (bg-cyan-intense "#284848")

    ;; Subtle backgrounds
    (bg-red-subtle "#3A2020")
    (bg-green-subtle "#203820")
    (bg-yellow-subtle "#383820")
    (bg-blue-subtle "#202840")
    (bg-magenta-subtle "#302038")
    (bg-cyan-subtle "#203838")

    ;; Diff backgrounds
    (bg-added "#203820")
    (bg-added-faint "#1A301A")
    (bg-added-refine "#284828")
    (fg-added "#90D090")

    (bg-changed "#383820")
    (bg-changed-faint "#302818")
    (bg-changed-refine "#484828")
    (fg-changed "#D0D080")

    (bg-removed "#3A2020")
    (bg-removed-faint "#301818")
    (bg-removed-refine "#4A2828")
    (fg-removed "#E09090")

    ;; Mode line
    (bg-mode-line-active "#384858")
    (fg-mode-line-active "#E8E8D0")

    ;; Special backgrounds
    (bg-completion "#303848")
    (bg-hover "#385050")
    (bg-hover-secondary "#304030")
    (bg-hl-line "#2A2A24")
    (bg-paren-match "#406060")
    (bg-err "#402020")
    (bg-warning "#383018")
    (bg-info "#203820")
    (bg-region "#484830")))

(defconst im-ef-acme-dark-palette-mappings-partial
  '((err red)
    (warning yellow)
    (info green)

    (fg-link blue-cooler)
    (fg-link-visited magenta)
    (name blue)
    (keybind blue-warmer)
    (identifier magenta-faint)
    (fg-prompt blue)

    (builtin magenta)
    (comment green-faint)
    (constant red-cooler)
    (fnname blue)
    (fnname-call blue-faint)
    (keyword blue)
    (preprocessor red)
    (docstring yellow-faint)
    (string red)
    (type cyan)
    (variable blue-warmer)
    (variable-use blue-faint)
    (rx-backslash cyan-cooler)
    (rx-construct red)

    (accent-0 blue)
    (accent-1 red)
    (accent-2 green)
    (accent-3 magenta)

    (date-common cyan)
    (date-deadline red)
    (date-deadline-subtle red-faint)
    (date-event fg-alt)
    (date-holiday magenta)
    (date-now fg-main)
    (date-range fg-alt)
    (date-scheduled yellow)
    (date-scheduled-subtle yellow-faint)
    (date-weekday cyan)
    (date-weekend red-faint)

    (fg-prose-code yellow)
    (prose-done green)
    (fg-prose-macro cyan)
    (prose-metadata fg-dim)
    (prose-metadata-value fg-alt)
    (prose-table fg-alt)
    (prose-table-formula info)
    (prose-tag yellow-faint)
    (prose-todo red)
    (fg-prose-verbatim magenta)

    (mail-cite-0 blue)
    (mail-cite-1 magenta)
    (mail-cite-2 green)
    (mail-cite-3 cyan)
    (mail-part magenta-faint)
    (mail-recipient blue)
    (mail-subject red)
    (mail-other cyan)

    (bg-search-static bg-yellow-intense)
    (bg-search-current bg-cyan-intense)
    (bg-search-lazy bg-blue-intense)
    (bg-search-replace bg-red-intense)

    (bg-search-rx-group-0 bg-magenta-intense)
    (bg-search-rx-group-1 bg-green-intense)
    (bg-search-rx-group-2 bg-red-subtle)
    (bg-search-rx-group-3 bg-cyan-subtle)

    (rainbow-0 blue)
    (rainbow-1 red)
    (rainbow-2 green)
    (rainbow-3 magenta)
    (rainbow-4 cyan)
    (rainbow-5 yellow)
    (rainbow-6 blue-warmer)
    (rainbow-7 red-cooler)
    (rainbow-8 green-cooler)))

(defcustom im-ef-acme-dark-palette-overrides nil
  "Overrides for `im-ef-acme-dark-palette'."
  :group 'ef-themes
  :package-version '(ef-themes . "1.0.0")
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(ef-themes) Palette overrides"))

(defconst im-ef-acme-dark-palette
  (modus-themes-generate-palette
   im-ef-acme-dark-palette-partial
   nil
   nil
   (append im-ef-acme-dark-palette-mappings-partial ef-themes-palette-common)))

(modus-themes-theme
 'im-ef-acme-dark
 'ef-themes
 "Legible dark theme with warm Acme/Plan 9 inspired colors."
 'dark
 'im-ef-acme-dark-palette
 nil
 'im-ef-acme-dark-palette-overrides
 'ef-themes-custom-faces)

(provide 'im-ef-acme-dark-theme)

;;; im-ef-acme-dark-theme.el ends here
