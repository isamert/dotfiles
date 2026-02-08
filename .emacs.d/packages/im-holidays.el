;;; im-holidays.el --- Some holiday definitions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; URL: https://github.com/isamert/dotfiles
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.2"))
;; Keywords: calendar

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Turkish and Dutch holiday definitions for Emacs calendar.

;;;; Usage

;; You probably would like to something like:
;;
;;   (setq calendar-holidays (append im-holidays-dutch-holidays im-holidays-turkish-holidays))
;;
;; Now you can use M-x holidays, or M-x holiday-list and see the
;; proper holidays.
;;
;; To display holidays marked in M-x calendar, do:
;;
;;   (setq calendar-mark-holidays-flag t)

;;; Code:

(defvar im-holidays-dutch-holidays
  '((holiday-fixed 1 1 "Nieuwjaarsdag (New Year's Day)")
    (holiday-easter-etc -2 "Goede vrijdag (Good Friday)")
    (holiday-easter-etc 0 "Eerste Paasdag (Easter Sunday)")
    (holiday-easter-etc 1 "Tweede Paasdag (Easter Monday)")
    (holiday-sexp
     '(let ((date (list 4 27 year)))
        (if (= (calendar-day-of-week date) 0)  ; Check if it's a Sunday
            (list 4 26 year)                   ; Shift to April 26 if April 27 is a Sunday
          date))
     "Koningsdag (King's Day)")
    (holiday-fixed 5 5 "Bevrijdingsdag (Liberation Day)")
    (holiday-easter-etc 39 "Hemelvaartsdag (Ascension Day)")
    (holiday-easter-etc 49 "Eerste Pinksterdag (Whit Sunday)")
    (holiday-easter-etc 50 "Tweede Pinksterdag (Whit Monday)")
    (holiday-fixed 12 25 "Eerste Kerstdag (Christmas Day)")
    (holiday-fixed 12 26 "Tweede Kerstdag (Boxing Day)")))

(defvar im-holidays-turkish-holidays
  '((holiday-fixed 1 1 "Yılbaşı")
    (holiday-islamic 9 29 "Ramazan Bayramı Arifesi")
    (holiday-islamic 10 1 "Ramazan Bayramı 1. Gün")
    (holiday-islamic 10 2 "Ramazan Bayramı 2. Gün")
    (holiday-islamic 10 3 "Ramazan Bayramı 3. Gün")
    (holiday-fixed 4 23 "Ulusal Egemenlik ve Çocuk Bayramı")
    (holiday-fixed 5 1 "Emek ve Dayanışma Günü")
    (holiday-fixed 5 19 "Atatürk'ü Anma, Gençlik ve Spor Bayramı")
    (holiday-islamic 12 9 "Kurban Bayramı Arifesi")
    (holiday-islamic 12 10 "Kurban Bayramı 1. Gün")
    (holiday-islamic 12 11 "Kurban Bayramı 2. Gün")
    (holiday-islamic 12 12 "Kurban Bayramı 3. Gün")
    (holiday-islamic 12 13 "Kurban Bayramı 4. Gün")
    (holiday-fixed 7 15 "Demokrasi Bayramı")
    (holiday-fixed 8 30 "Zafer Bayramı")
    (holiday-fixed 10 28 "Cumhuriyet Bayramı Arifesi")
    (holiday-fixed 10 29 "Cumhuriyet Bayramı")
    (holiday-fixed 12 31 "Yılbaşı Gecesi")))

;;;; Footer

(provide 'im-holidays)
;;; im-holidays.el ends here
