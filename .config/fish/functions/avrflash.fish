function avrflash
    mkdir build
    cd build
    avr-gcc ../$argv -Os -Wall -mmcu=atmega32 -o main_bin 
    avr-objcopy -j .text -j .data -O ihex main_bin "main.hex"
    avrdude -c usbasp -p m32 -U flash:w:"main.hex"
    cd ..
end
                        
