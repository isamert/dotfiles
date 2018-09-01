function boxdownload
    echo "If you set BOXPASSWORD variable, it will not ask password for this session."

    if test -n "$BOXPASSWORD"
        curl -u "isamertgurbuz@gmail.com:$BOXPASSWORD" https://dav.box.com/dav/$argv[1] --output $argv[2]
    else
        curl -u "isamertgurbuz@gmail.com" https://dav.box.com/dav/$argv[1] --output $argv[2]
    end
end
