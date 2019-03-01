#!/bin/bash
DEVICE="tpacpi::kbd_backlight"
backlight=$(xbacklight -ctrl ${DEVICE} -get)
if [ $backlight -eq 0 ]; then
    xbacklight -ctrl ${DEVICE} -set 100
else
    xbacklight -ctrl ${DEVICE} -set 0
fi
