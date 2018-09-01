function boxupload
	echo "If you set BOXPASSWORD variable, it will not ask password for this session."

    if test -n "$BOXPASSWORD"
        curl -u "isamertgurbuz@gmail.com:$BOXPASSWORD" -T $argv[1] https://dav.box.com/dav/$argv[2]
    else
        curl -u isamertgurbuz@gmail.com -T $argv[1] https://dav.box.com/dav/$argv[2]
    end
end
