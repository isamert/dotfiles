;; I don't endorse anything below, I just follow them because either I
;; like them or I'm just curious. There are some stuff that I hate but
;; I also like to know what's been going on

(defvar isamert/feeds
  `((,isamert/github-feed-link personal)
    ("https://mail.google.com/mail/feed/atom" personal)

    ("https://t24.com.tr/rss/haber/gundem" news turkish)
    ("https://t24.com.tr/rss/haber/politika" news turkish)
    ("https://t24.com.tr/rss/haber/dunya" news turkish)

    ("https://hnrss.org/frontpage" news)

    ("https://www.archlinux.org/feeds/news/" update linux)
    ("https://blog.tecosaur.com/tmio/rss.xml" update emacs)

    ("http://aecepoglu.com/posts/index.xml" blog dev turkish soostone)
    ("http://nullprogram.com/feed/" blog dev)
    ("http://xenodium.com/rss.xml" blog dev)
    ("https://eli.thegreenplace.net/feeds/all.atom.xml" blog dev)
    ("https://drewdevault.com/feed.xml" blog dev)
    ("https://haskell-explained.gitlab.io/blog/feeds/rss.xml" blog dev haskell)
    ("http://www.haskellforall.com/feeds/posts/default" blog dev haskell)
    ("https://chrispenner.ca/atom.xml" blog dev haskell)
    ("https://emacsredux.com/atom.xml" blog emacs)
    ("https://lexi-lambda.github.io/feeds/all.rss.xml" blog dev haskell)
    ("https://jaspervdj.be/rss.xml" blog dev haskell)
    ("https://blog.m-ou.se/index.xml" blog dev rust)
    ("http://xion.io/feeds/atom.xml" blog dev rust)
    ("https://ag91.github.io/rss.xml" blog dev emacs)
    ("https://chrisdone.com/rss.xml" blog dev haskell)
    ("https://notes.srid.ca/blog.xml" blog dev haskell)
    ("https://two-wrongs.com/feed" blog dev)
    ("https://beepb00p.xyz/rss.xml" blog dev)
    ("https://www.rousette.org.uk/index.xml" blog dev emacs essay)
    ("https://jao.io/blog/rss.xml" blog dev emacs)
    ("https://old.reddit.com/r/gwern/search.rss?q=flair:'Gwern'&sort=new&restrict_sr=on" blog essay)
    ("https://www.lesswrong.com/feed.xml?view=curated-rss" blog essay)
    ("https://writings.stephenwolfram.com/feed/" blog essay)
    ("http://explosm.net/rss" comic)

    ("BlackMetalUpdates" youtube music metal black)
    ("XOdiumNostrumX" youtube music metal extreme)
    ("ForgottenChants" youtube music metal extreme folk)
    ("UCUIIM9pPaSdRg7xquOX7s-w" youtube music metal power) ;; Unknown Power Metal YT
    ("UCzCWehBejA23yEz3zp7jlcg" youtube music metal black) ;; Black Metal Promotion
    ("UCZQHDXu4JvsmptR86XDyaRQ" youtube music metal heavy classic) ;; Rare & Obsucre Metal Archives

    ("commandline" reddit linux cli)
    ("orgmode" reddit emacs)
    ("emacs" reddit emacs)
    ("bspwm" reddit linux)

    ("UCyoQK-mZXr2ws4C0nXGCH1w" youtube philosophy) ;; Zero Books
    ("UCSkzHxIcfoEr69MWBdo0ppg" youtube philosophy) ;; Cuck philosophy
    ("thephilosophytube" youtube fun philosophy)
    ("ContraPoints" youtube fun philosophy)
    ("UC738SsV6BSLUVvMgKnEFFzQ" youtube philosophy) ;; Epoch Philosophy
    ("ShaunandnotJen" youtube philosophy commentary) ;; Shaun
    ("UCkS_HP3m9NXOgswVAKbMeJQ" youtube philosophy) ;; Then & Now
    ("UCjnpuIGovFFUBLG5BeHzTag" youtube philosophy podcast) ;; Philosophize This
    ("aliabdaal" youtube vlog)
    ("UCYO_jab_esuFRV4b17AJtAw" youtube fun science animation) ; 3Blue1Brown
    ("UCsXVk37bltHxD1rDPwtNM8Q" youtube fun science animation) ;; Kurzgesagt – In a Nutshel
    ("numberphile" youtube fun science)
    ("UC0uTPqBCFIpZxlz_Lv1tk_g" youtube emacs dev) ;; Protesilaos Stavrou
    ("UC2NXVB__-v-hf-l6wcg1zjQ" youtube linux dev cli) ;; Tom on the internet
    ("UC2eYFnH61tmytImy1mTYvhA" youtube linux cli) ;; Luke Smith
    ("OmegaDungeon" youtube linux cli) ;; Brodie Robertson
    ("UCEbYhDd6c6vngsF5PQpFVWg" youtube dev haskell) ;; Tsoding
    ("UCUgxpaK7ySR-z6AXA5-uDuw" youtube dev haskell) ;; Haskell at Work
    ("MentalOutlawStudios" youtube cli linux)
    ("UCbnGhYfkxiZdOPrZg4RBevw" youtube emacs cli linux) ;; Ravar's Path
    ("polyglut" youtube emacs) ;; Xah Lee
    ("babyWOGUE" youtube linux fun)
    ("UC8ENHE5xdFSwx71u3fDH5Xw" youtube vim cli dev) ;; ThePrimeagen
    ("mzamansky" youtube emacs)
    ("YourMovieSucksDOTorg" youtube fun movie)
    ("RedLetterMedia" youtube fun movie)
    ("MrDanielmac" youtube movie)
    ("CineFix" youtube movie fun)
    ("hotbananastud" youtube fun skit) ;; Brandon Rogers
    ("ilkercanikligil" youtube fun turkce) ;; Flu TV
    ("199sokak" youtube fun turkish skit) ;; Kamusal Mizah
    ("UC3Xu0FQeizzXhUtthFjFs8g" youtube fun skit vlog) ;; Deniz Bagdas
    ("codyko69" youtube fun commentary)
    ("conmech" youtube fun turkce) ;; Efe Aydal
    ("tvMisha" youtube fun turkce vlog)

    ("UCWXCrItCF6ZgXrdozUS-Idw" youtube fun animation) ; ExplosmEntertainment
    ))
