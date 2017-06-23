# module-DateTime
A simple Fortran 90 Date Module to mimic the date functions in MATLAB 

        !----------------------------------------------------------------------------------------------------------
        !Author: Chonghua Yin
        !Date: 19-Arp-2013
        !Description: This module provides 9 public methods, which simulate date functions in matlab.
    
        !Details as follows:
        !datenum:   will convert a date into serial number.
        !datevec:   will convert back a date serial number into year, month, day, hour, minute and second.
        !datestr:   will convert a year, month and day into string like yyyymmdd.
        !addtodate: will allow to add number of (year, month, day, hour, minute or second) to the known date
        !           serial number to get a new date serial number.
        !parsedate: will convert date number series into real year month day according to different calendars
        !           currently, only support 'standard', 'gregorian', 'proleptic_gregorian', 'noleap', '365_day'
        !                                   'all_leap', '366_day', '360_day'
        !getDatestrFromASentence: 
        !           will extract a yyyy-mm-dd like string from a sentence.
        !extractDateFromDatestr:
        !           will extract year, month, day from a yyyy-mm-dd date string
        !monthdays: will get the days in a month that specified by a date serial number or specific year and month
        !leapyear:  will check if it is a leap year for a specific year.
        !-----------------------------------------------------------------------------------------------------------
