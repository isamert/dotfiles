function nameof --description 'Get the name of given PID'
	ps -p $argv -o comm=
end
