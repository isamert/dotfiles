;;; im-feeds.el --- My feed/rss list -*- lexical-binding: t; -*-

;;; Commentary:

;; I don't endorse anything below, I just follow them because either I
;; like them or I'm just curious.  There are some stuff that I hate
;; but I also like to know what's been going on.

;;; Code:

(require 'im-secrets)

(defvar im-feeds
  `((,im-github-feed-link personal)
    (,im-reddit-feed-link personal)
    ("https://mail.google.com/mail/feed/atom" personal)
    ;; ^ This requires an app password set in ~/.netrc

    ("https://t24.com.tr/rss/haber/gundem" news turkish)
    ("https://t24.com.tr/rss/haber/politika" news turkish)
    ("https://t24.com.tr/rss/haber/dunya" news turkish)
    ("https://t24.com.tr/rss/haber/ekonomi" news turkish)

    ("https://hnrss.org/frontpage" news)

    ("https://www.archlinux.org/feeds/news/" update linux)
    ("https://blog.tecosaur.com/tmio/rss.xml" update emacs)

    ("https://karl-voit.at/feeds/lazyblorg-all.atom_1.0.links-only.xml" blog pim emacs)
    ("http://aecepoglu.com/posts/index.xml" blog dev turkish soostone)
    ("http://nullprogram.com/feed/" blog dev)
    ("http://xenodium.com/rss.xml" blog dev)
    ("https://karthinks.com/index.xml" blog dev emacs)
    ("https://eli.thegreenplace.net/feeds/all.atom.xml" blog dev)
    ("https://drewdevault.com/feed.xml" blog dev)
    ;; DOWN: ("https://haskell-explained.gitlab.io/blog/feeds/rss.xml" blog dev haskell)
    ("http://www.haskellforall.com/feeds/posts/default" blog dev haskell)
    ("https://chrispenner.ca/atom.xml" blog dev haskell)
    ("https://emacsredux.com/atom.xml" blog emacs)
    ("https://lexi-lambda.github.io/feeds/all.rss.xml" blog dev haskell)
    ("https://jaspervdj.be/rss.xml" blog dev haskell)
    ("https://blog.m-ou.se/index.xml" blog dev rust)
    ("http://xion.io/feeds/atom.xml" blog dev rust)
    ("https://ag91.github.io/rss.xml" blog dev emacs)
    ("https://chrisdone.com/rss.xml" blog dev haskell)
    ;; DOWN: ("https://notes.srid.ca/blog.xml" blog dev haskell)
    ("https://two-wrongs.com/feed" blog dev)
    ("https://beepb00p.xyz/rss.xml" blog dev)
    ("https://www.rousette.org.uk/index.xml" blog dev emacs essay)
    ("https://jao.io/blog/rss.xml" blog dev emacs)
    ;; DOWN: ("https://psachin.gitlab.io/index.xml" blog dev emacs lisp)
    ("https://fuco1.github.io/rss.xml" blog dev emacs lisp)
    ;; DOWN: ("https://old.reddit.com/r/gwern/search.rss?q=flair:'Gwern'&sort=new&restrict_sr=on" blog essay) ;; Gwern
    ("https://www.lesswrong.com/feed.xml?view=curated-rss" blog essay)
    ("https://writings.stephenwolfram.com/feed/" blog essay)
    ("https://astralcodexten.substack.com/feed" blog essay) ;; Scott Alexander
    ("https://escapingflatland.substack.com/feed" blog essay) ;; Henrik Karlsson
    ("https://www.theinsight.org/feed" blog essay) ;; Zeynep Tufekci
    ("https://medium.com/feed/incerto" blog essay) ;; Nassim Nicholas Taleb
    ("https://www.cold-takes.com/rss" blog essay)
    ;; ("https://jabberwocking.com/feed/" blog essay) ;; Kevin Drum
    ("https://fatmagulunyengesi.substack.com/feed" blog essay turkish)
    ("https://lukesmith.xyz/rss.xml" blog linux cli)

    ("BlackMetalUpdates" youtube music metal black)
    ("XOdiumNostrumX" youtube music metal extreme)
    ("ForgottenChants" youtube music metal extreme folk)
    ("UCUIIM9pPaSdRg7xquOX7s-w" youtube music metal power) ;; Unknown Power Metal YT
    ("UCZQHDXu4JvsmptR86XDyaRQ" youtube music metal heavy classic) ;; Rare & Obsucre Metal Archives
    ("UCU7JpmRu-T0XdDseYb-JSTg" youtube music metal death) ;;UnknownMelodeath
    ("BangerTV" youtube music review)

    ("commandline" reddit linux cli)
    ("selfhosted" reddit linux cli)
    ("orgmode" reddit emacs)
    ("emacs" reddit emacs)
    ("bspwm" reddit linux)

    ("UCyoQK-mZXr2ws4C0nXGCH1w" youtube philosophy) ;; Zero Books
    ("UCSkzHxIcfoEr69MWBdo0ppg" youtube philosophy) ;; Cuck philosophy
    ("thephilosophytube" youtube fun philosophy)
    ("UCNvsIonJdJ5E4EXMa65VYpA" youtube fun philosophy) ;; ContraPoints
    ("UC738SsV6BSLUVvMgKnEFFzQ" youtube philosophy) ;; Epoch Philosophy
    ("UCJ6o36XL0CpYb6U5dNBiXHQ" youtube philosophy commentary) ;; Shaun
    ("UCkS_HP3m9NXOgswVAKbMeJQ" youtube philosophy) ;; Then & Now
    ("UCjnpuIGovFFUBLG5BeHzTag" youtube philosophy podcast) ;; Philosophize This
    ("UCsXVk37bltHxD1rDPwtNM8Q" youtube fun science animation) ;; Kurzgesagt – In a Nutshel
    ("UCCT8a7d6S6RJUivBgNRsiYg" youtube fun philosophy) ;; Three Arrows
    ("numberphile" youtube fun science)
    ("UC0uTPqBCFIpZxlz_Lv1tk_g" youtube emacs dev) ;; Protesilaos Stavrou
    ("UC2NXVB__-v-hf-l6wcg1zjQ" youtube linux dev cli) ;; Tom on the internet
    ("UC2eYFnH61tmytImy1mTYvhA" youtube linux cli) ;; Luke Smith
    ("UCEbYhDd6c6vngsF5PQpFVWg" youtube dev haskell) ;; Tsoding
    ("UCUgxpaK7ySR-z6AXA5-uDuw" youtube dev haskell) ;; Haskell at Work
    ("MentalOutlawStudios" youtube cli linux)
    ("UCbnGhYfkxiZdOPrZg4RBevw" youtube emacs cli linux) ;; Ravar's Path
    ("polyglut" youtube emacs) ;; Xah Lee
    ("UCZWadyLVO4ZnMgLrRVtS6VA" youtube linux fun) ;; babyWOGUE
    ("UC8ENHE5xdFSwx71u3fDH5Xw" youtube vim cli dev) ;; ThePrimeagen
    ("UCS4FAVeYW_IaZqAbqhlvxlA" youtube dev) ;; Context-Free
    ("UCkf4VIqu3Acnfzuk3kRIFwA" youtube cli) ;; gotbletu
    ("mzamansky" youtube emacs)
    ("YourMovieSucksDOTorg" youtube fun movie)
    ("UC9DkCKm4_VDztRRyge4mCJQ" youtube fun music) ;; Amoeba
    ("RedLetterMedia" youtube fun movie)
    ("MrDanielmac" youtube movie)
    ("CineFix" youtube movie fun)
    ("UCn8zNIfYAQNdrFRrr8oibKw" youtube news documentary) ;; VICE
    ("hotbananastud" youtube fun skit) ;; Brandon Rogers
    ("199sokak" youtube fun turkish skit) ;; Kamusal Mizah
    ("UC3Xu0FQeizzXhUtthFjFs8g" youtube fun skit vlog) ;; Deniz Bagdas
    ("UCGSGPehp0RWfca-kENgBJ9Q" youtube fun skit) ;; jREG
    ("codyko69" youtube fun commentary)
    ("UCZRVHEVslkXoqXEYwJhlTxg" youtube fun turkish vlog) ;; Misha Defne
    ("UCB12vt2obajvkegtlpb_F8Q" youtube fun turkish) ;; Kalt
    ("UCMixjxEx6t663lIf0aQBeSg" youtube turkish) ;; Sevan Nisanyan

    ;; Software tracking
    ("alphapapa/org-ql" gh-release app)))

;; For easy updating:
;; (setq elfeed-feeds (mapcar #'im-elfeed--expand im-feeds))

(defun im-get-youtube-channel-id-from-video-link (link-or-id)
  "Get YouTube channel id from given video LINK-OR-ID.
Using channel id is more reliable for retrieving videos with RSS."
  (interactive "sVideo link/id: ")
  (message
   "Copied: %s"
   (im-kill
    (let-alist (im-request (format "%s/videos/%s" empv-invidious-instance (im-youtube-link-extract-id link-or-id)))
      .authorId))))

(provide 'im-feeds)
;;; im-feeds.el ends here
