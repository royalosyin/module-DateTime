    module DateTime
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
        implicit none

        public :: datenum, datevec, datestr, addtodate, monthdays, leapyear
        public :: parsedate, getDatestrFromASentence, extractDateFromDatestr
        private  
            integer :: dayhour = 24
            integer :: daymin  = 1440
            integer :: daysec  = 86400
            integer :: hoursec = 3600
            integer :: minsec  = 60
            integer :: y400day = 146097 ! number of days in 400 years

        ! returns logical value whether a time is leap year or not
        interface leapyear
            module procedure leapyear_datenum ! requires a double arg.
            module procedure leapyear_year    ! requires an integer arg.
        end interface

        ! returns a number of days in a month of a year
        interface monthdays
            module procedure monthdays_datenum    ! requires a double arg.
            module procedure monthdays_year_month ! requires two integer args.
        end interface

        ! convert year, month, day into string like yyyymmdd
        interface datestr
            module procedure datestr_datenum        ! requires a double arg.
            module procedure datestr_year_month_day ! requires 3 integer args.
        end interface

        contains
        !--------------------------------------------------------------------
        ! Public subroutines
        !-------------------------------------------------------------------

        !--------------------------------------------------------------------------------------------------
        ! Calculate the number of days from a reference date of
        ! 0 Jan, 0000 as a double precision real number.
        !--------------------------------------------------------------------------------------------------
        recursive subroutine datenum( &
            this, year, month, day, hour, minute, second)
            implicit none
            double precision, intent(out)          :: this
            integer,          intent(in), optional :: year, month,  day
            integer,          intent(in), optional :: hour, minute, second
            integer :: date_arr(8)
            integer :: wyear, wmonth, wday

            this = 0.0d0

            ! return current time
            if(.not.present(year)) then
                call date_and_time(values=date_arr)
                call datenum(this, date_arr(1), date_arr(2), date_arr(3), &
                             date_arr(5), date_arr(6), date_arr(7))
                return
            endif

            ! add days less than a day
            if(present(second)) this = this + dble(second)/dble(daysec)
            if(present(minute)) this = this + dble(minute)/dble(daymin)
            if(present(hour))   this = this + dble(hour)  /dble(dayhour) 

            ! add days in a month
            if(.not.present(day)) then
            this = this + 1.0d0
            else
                do wday = 1, day
                    this = this + 1.0d0
                enddo
            endif

            ! add days before month
            if(present(month)) then
                do wmonth = 1, month-1
                    this = this + dble(monthdays(year, wmonth))
                enddo
            endif

            ! add days before year
            wyear = 0
            do while(year-wyear >= 400)
                wyear = wyear + 400
                this = this + dble(y400day)
            enddo

            do while(wyear<year)
                if(leapyear(wyear)) then
                    this = this + 366.0d0
                else
                    this = this + 365.0d0
                endif
                wyear = wyear + 1
            enddo 

        end subroutine
  
        !------------------------------------------------------------------------------------------------
        ! Returns elements of a date as integers.
        !-----------------------------------------------------------------------------------------------
        subroutine datevec(datenum, year, month, day, hour, minute, second)
            implicit none
            double precision, intent(in)            :: datenum
            integer,          intent(out), optional :: year, month, day
            integer,          intent(out), optional :: hour, minute, second
            integer :: wyear, wmonth, wday, whour, wminute, wsecond ! for working
            integer :: seconds, days ! elapsed time from a reference time

            ! parse time less than a day
            seconds = nint((datenum - floor(datenum))*dble(daysec))
            whour   = int(seconds/hoursec)
            seconds = seconds - whour*hoursec
            wminute = int(seconds/minsec)
            wsecond = seconds - wminute*minsec
    
            if(datenum<1.0d0) then
                wyear = 0
                wmonth = 1
                wday = 0
            else
                wyear = 0
                wmonth = 1
                wday = 1
                days = 1

                do while(int(datenum) - days >= y400day)
                    wyear = wyear + 400
                    days = days + y400day
                enddo
  
                do while(int(datenum) - days >= 366)
                    if(leapyear(wyear)) then
                        days = days + 366
                    else
                        days = days + 365
                    endif
                        wyear = wyear + 1
                enddo
  
                do while(days<int(datenum))
                    wday = wday + 1
                    if(wday>monthdays(wyear, wmonth)) then
                        wday = 1
                        wmonth = wmonth + 1
                
                        if(wmonth>12) then
                            wmonth = 1
                            wyear = wyear + 1
                        endif
                    endif
            
                    days = days + 1
                enddo
            endif

            if(present(year))   year   = wyear
            if(present(month))  month  = wmonth
            if(present(day))    day    = wday
            if(present(hour))   hour   = whour
            if(present(minute)) minute = wminute
            if(present(second)) second = wsecond

        end subroutine

        !--------------------------------------------------------------------------------------------------
        ! Adds number of days to a date.
        !--------------------------------------------------------------------------------------------------
        subroutine addtodate(datenum, num, unit)
            implicit none
            double precision, intent(inout) :: datenum
            integer,           intent(in)    :: num
            character(*),      intent(in)    :: unit
            if(trim(unit).eq.'second') datenum = datenum + dble(num)/dble(daysec)
            if(trim(unit).eq.'minute') datenum = datenum + dble(num)/dble(daymin)
            if(trim(unit).eq.'hour')   datenum = datenum + dble(num)/dble(dayhour)
            if(trim(unit).eq.'day')    datenum = datenum + dble(num)
        end subroutine 


        !--------------------------------------------------------------------------------------------------
        ! convert date numbers into real date (year,month,day) According to different calendars.
        ! -------------------------------------------------------------------------------------------------
        subroutine parsedate(serial_rel, serial_base, calendar, serial_date)
	        double precision, dimension(:), intent(in):: serial_rel
	        double precision, intent(in):: serial_base
	        character(len=*), intent(in):: calendar
	        integer, dimension(:,:), intent(out):: serial_date
	
	        integer ndate, ncols, idate, iyear, imonth, iday, imon, imons
	        integer year_b, month_b, day_b
	        double precision :: serial_time(size(serial_rel,1)), day_full(size(serial_rel,1))
	        double precision :: rem_1(size(serial_rel,1)), rem_2(size(serial_rel,1))
	        double precision :: days_from_year_base
	        integer year_rel(size(serial_rel,1)), year_abs(size(serial_rel,1))
	        integer month_abs(size(serial_rel,1)), day_rel(size(serial_rel,1)), day_abs(size(serial_rel,1))
	        integer days_per_month(12), days_ref(13)
	
	        ndate = size(serial_rel,1)
	        ncols = size(serial_date, 2)
	
	        select case(calendar)
		        case ('standard', 'gregorian', 'proleptic_gregorian')			
			        serial_time = serial_rel + serial_base			
			        ! using my datevec, how to do iteration			
			        do idate = 1, ndate
				        call datevec(serial_time(idate), iyear, imonth, iday)
				        serial_date(idate, 1) = iyear
				        serial_date(idate, 2) = imonth
				        serial_date(idate, 3) = iday
			        enddo
			
		        case ('noleap', '365_day')
			        ! We use serial_base to give us a proper starting time and work from
			        ! there in steps of 365 days per year.
			        days_per_month = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
			        days_ref       = (/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365/)
			
			        call datevec(serial_base, year_b, month_b, day_b)			
			        days_from_year_base = days_ref(month_b) + day_b - 1 
			        day_full = serial_rel + days_from_year_base
			
			        do idate = 1, ndate
				        year_rel(idate) = floor(day_full(idate)/365);
				        year_abs(idate) = year_b + year_rel(idate);
				        rem_1(idate)    = day_full(idate) - year_rel(idate)*365
			        enddo
			
			        month_abs = 0
			        do idate = 1, ndate 
				        imons = 0
				        do imon = 1, 13
					        !ff = find(days_ref <= rem_1(idate));
					        if(days_ref(imon) <= rem_1(idate)) then
						        imons = imon
					        endif
				        enddo
				
				        !month_abs(idate) = ff(end);
				        month_abs(idate) = imons
				        rem_2(idate) = rem_1(idate) - days_ref(month_abs(idate))
			        enddo			
			
			        do idate = 1, ndate
				        day_rel(idate) = floor(rem_2(idate))				
			        enddo
			
			        day_abs = day_rel + 1
			
			        do idate = 1, ndate
				        serial_date(idate, 1) = year_abs(idate)
				        serial_date(idate, 2) = month_abs(idate)
				        serial_date(idate, 3) = day_abs(idate)
			        enddo
			
		        case ('all_leap', '366_day')
			        ! We use serial_base to give us a proper starting time and work from
			        ! there in steps of 366 days per year.
			        days_per_month = (/31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
			        days_ref       = (/0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366/)
			
			        call datevec(serial_base, year_b, month_b, day_b)
			
			        days_from_year_base = days_ref(month_b) + day_b - 1
			        day_full = serial_rel + days_from_year_base
			
			        do idate = 1, ndate
				        year_rel(idate) = floor(day_full(idate)/366)
			        enddo
			
			        year_abs = year_b + year_rel
			        rem_1 = day_full - year_rel*366
			
			        month_abs = 0
			        do idate = 1, ndate 
				        imons = 0
				        do imon = 1, 13
					        !ff = find(days_ref <= rem_1(idate));
					        if(days_ref(imon) <= rem_1(idate)) then
						        imons = imon
					        endif
				        enddo
				
				        !month_abs(idate) = ff(end);
				        month_abs(idate) = imons
				        rem_2(idate) = rem_1(idate) - days_ref(month_abs(idate))
			        enddo			
			
			        do idate = 1, ndate
				        day_rel(idate) = floor(rem_2(idate))				
			        enddo
			
			        day_abs = day_rel + 1
			
			        do idate = 1, ndate
				        serial_date(idate, 1) = year_abs(idate)
				        serial_date(idate, 2) = month_abs(idate)
				        serial_date(idate, 3) = day_abs(idate)
			        enddo

		        case ('360_day')
			        ! We use serial_base to give us a proper starting time and work from
			        ! there in steps of 366 days per year.
			        call datevec(serial_base, year_b, month_b, day_b)
			
			        days_from_year_base = 30*(month_b - 1) + day_b - 1
			        day_full = serial_rel + days_from_year_base
			
			        do idate = 1, ndate
				        year_rel(idate) = floor(day_full(idate)/360)
			        enddo
			        year_abs = year_b + year_rel
			        rem_1    = day_full - year_rel*360
			
			        do idate = 1, ndate
				        month_abs(idate) = floor(rem_1(idate)/30) + 1
				        rem_2(idate)     = rem_1(idate) - (month_abs(idate) - 1)*30
				        day_rel(idate)   = floor(rem_2(idate))
			        enddo			
			
			        day_abs = day_rel + 1
			
			        do idate = 1, ndate
				        serial_date(idate, 1) = year_abs(idate)
				        serial_date(idate, 2) = month_abs(idate)
				        serial_date(idate, 3) = day_abs(idate)
			        enddo
			
		        case DEFAULT
                    write (*,*) "Your calendar is not supported now."
                    
                    do idate = 1, ndate
                        serial_date(idate, 1) = idate
				        serial_date(idate, 2) = idate
				        serial_date(idate, 3) = idate
                    enddo
	        end select
        end subroutine

        !----------------------------------------------------------------------------------------------------
        ! Get yyyy-mm-dd date string from a sentence. It only havs limited support. Eg., A sentence must have 
        ! a only date string wihtout other numbers.I will change this functioin in the future.
        ! ---------------------------------------------------------------------------------------------------
        Subroutine getDatestrFromASentence(str)
            implicit none 

            ! Removes spaces, tabs, and control characters in string str
            character(len=*), intent(inout):: str
        
            character(len=1):: ch
            character(len=len_trim(str)):: outstr     
            integer lens, i      

            ! First convert letter into space, but keep '-'
            str    = adjustl(str)
            lens   = len_trim(str)
            outstr = str(1:lens)      
        
            do i = 1, lens
                ch  = str(i:i)
           
                if(.NOT. is_digit(ch) .AND. ch/='-'  ) then
                    outstr(i:i) = ' '
                end if 
            enddo

            call removesp(outstr)  
        
            str = outstr
           
        contains       
       
            ! Returns .true. if ch is a digit (0,1,...,9) and .false. otherwise
            function is_digit(ch)              
                character :: ch
                logical :: res, is_digit

                select case(ch)
                    case('0':'9')
                        res  = .true.
                    case default
                        res = .false.
                end select

                is_digit = res
      
            end function is_digit

            ! Removes spaces, tabs, and control characters in string str
            subroutine removesp(str)              
                character(len=*):: str
                character(len=1):: ch
                character(len=len_trim(str)):: outstr

                integer lenstr, k, i, ich

                str    = adjustl(str)
                lenstr = len_trim(str)
                outstr = ' '
                k      = 0

                do i = 1, lenstr
                    ch  = str(i:i)
                    ich = iachar(ch)
            
                    select case(ich)    
                        case(0:32)  ! space, tab, or control character
                            cycle  
                             
                        case(33:)  
                            k = k + 1
                            outstr(k:k) = ch

                    end select
                end do

                str = adjustl(outstr)
            end subroutine removesp       

        End Subroutine GetDatestrFromASentence

        !---------------------------------------------------------------------------------------------------------
        ! Date Format Verification Routine
        !    Verifies date is in proper format, extracts month, day, year into integer variables,
        !    returns the day of the week, and returns an error code
        !
        !    date:  date in character format "yyyy-mm-dd"
        !    imonth:  the returned month value in integer format (range: 1-12)
        !    iday:  the returned day of the month value in integer format (range: 1-month total)
        !    iyear: the returned year value in integer format (range: 1-9999)
        !    idayofweek:  the returned day of the week (range: 1-7, 1=Sunday)
        !    idayofyear:  the returned day of the year (range:  1-year total)
        !    iret:  0 = successful
        !          -1 = too many characters entered
        !          -2 = unknown error, invalid format (e.g. no slashes)
        !          -3 = non-numeric data for month
        !          -4 = non-numeric data for day
        !          -5 = non-numeric data for year
        !          -6 = month value out of range (1-12)
        !          -7 = day value out of range (1-month maximum)
        !          -8 = year value out of range (1-9999)
        !subroutine extractDateFromDatestr(date, iyear, imonth, iday, idayofweek, idayofyear, iret)
        !----------------------------------------------------------------------------------------------------------
        subroutine extractDateFromDatestr(date, iyear, imonth, iday, iret)
            implicit none

            character(*), intent (in) :: date
            character(2)               :: workmonth, workday
            character(4)               :: workyear
            character(20)              :: workdate

            integer, intent(out)      :: imonth, iday, iyear ,iret !idayofweek, idayofyear
            integer                    :: iom, iod, ioy, lenstr, i, months(12)
            integer                    :: ia, im, iy

            logical                    :: leapyear, centyear
        
            ! Clear return code        
            iret = 0
       
            ! Extract a chunk to work on (a little more than necessary to allow some error checking)
            workdate = adjustl(date)

            ! Remove nulls if present
            do i = 1, len_trim(workdate)
                if (workdate(i:i) .eq. char(0)) workdate(i:i) = ' '
            end do

            lenstr = len_trim(workdate)

            ! Check number of characters input
            if (lenstr .gt. 10) then
                iret = -1 !Too many characters
                return
            end if

            ! Too few characters entered?
            if (lenstr .lt. 10) then
                if (workdate(7:7) .eq. '-') workdate = workdate(1:5)// "0" // workdate(6:)
                if (workdate(10:10) .eq. ' ') workdate = workdate(1:8) // "0" // workdate(9:)
            end if

            lenstr = len_trim(workdate)

            if (lenstr .lt. 10) then
                iret = -2 !some other error, this should not occur unless no "/" entered
                return
            end if

            workyear  = workdate(1:4)
            workmonth = workdate(6:7)
            workday   = workdate(9:10)       

            read(workyear, '(i4)', iostat=ioy) iyear
            read(workmonth,'(i2)', iostat=iom) imonth
            read(workday,  '(i2)', iostat=iod) iday

            if (iom .ne. 0) then !(probably) non-numeric data entered for month
                iret = -3
                return
            end if

            if (iod .ne. 0) then !non-numeric data entered for day
                iret = -4
                return
            end if

            if (ioy .ne. 0) then !non-numeric data entered for year
                iret = -5
                return
            end if

            if (imonth .lt. 1 .or. imonth .gt. 12) then !month out of range
                iret = -6
                return
            end if

            if (iyear .lt. 1 .or. iyear .gt. 9999) then !year out of range
                iret = -8
                return
            end if

            ! Determine if it is a leap year!
            centyear = .false.
            leapyear = .false.

            if (iyear / 100. - int(iyear / 100.) .eq. 0) centyear = .true. !Century year?
            if (iyear / 4. - int(iyear / 4.) .eq. 0) leapyear = .true.     !Leap year unless...

            if (centyear) then
                if (iyear / 400. - int(iyear / 400.) .gt. 0) leapyear = .false. !Century year not divisible by 400
            end if

            ! Length of each month for use later
            months     = 31 !all to 31

            months(02) = 28              !exceptions, Feb
            if (leapyear) months(2) = 29 !Feb in leap year

            months(04) = 30 !Apr
            months(06) = 30 !Jun
            months(09) = 30 !Sep
            months(11) = 30 !Nov

            select case (imonth) !Leap year and other processing for days of month

            case(4, 6, 9, 11) !Apr, Jun, Sep, Nov
                if (iday .lt. 1 .or. iday .gt. 30) then
                    iret = -7
                    return
                end if

            case(1, 3, 5, 7, 8, 10, 12) !Mar, May, Jul, Aug, Oct, Dec
                if (iday .lt. 1 .or. iday .gt. 31) then
                    iret = -7
                    return
                end if

            case(2) !Feb
                if (leapyear) then
                    if (iday .lt. 1 .or. iday .gt. 29) then
                        iret = -7
                        return
                    end if
                else
                    if (iday .lt. 1 .or. iday .gt. 28) then
                        iret = -7
                        return
                    end if
                end if

            end select

            ! The following functions were canceled. No support any more.
            ! Calculate day of the week, adjusted original algorithm from 0-6 to 1-7 for convenience)
            !ia = (14 - imonth) / 12
            !iy = iyear - ia
            !im = imonth + (12 * ia) - 2
            !idayofweek = mod(idayofyear + iy + (iy/4) - (iy/100) + (iy/400) + (31*im) + 4,7) + 1

            ! Calculate day of the year
            !do i = 1,imonth-1
            !    idayofyear = idayofyear + months(i)
            !end do

            !idayofyear = idayofyear + iday
            return
        end subroutine


        !--------------------------------------------------------
        ! Private functions
        !--------------------------------------------------------
        !---------------------------------------------------------------------------------------------
        ! Returns logical value whether a datenum is a leap year or not
        !----------------------------------------------------------------------------------------------
        logical function leapyear_datenum(datenum)
            double precision, intent(in) :: datenum
            integer :: year
        
            call datevec(datenum, year)
            if(mod(year, 400) == 0) then
                leapyear_datenum = .true.
            else if(mod(year, 100) == 0) then
                leapyear_datenum = .false.
            else if(mod(year, 4) == 0) then
                leapyear_datenum = .true.
            else
                leapyear_datenum = .false.
            endif
        end function

        !-------------------------------------------------------------------------------------------
        ! Returns logical value whether a year is a leap year or not
        !-------------------------------------------------------------------------------------------
        logical function leapyear_year(year)
            integer, intent(in) :: year
        
            if(mod(year, 400) == 0) then
                leapyear_year = .true.
            else if(mod(year, 100) == 0) then
                leapyear_year = .false.
            else if(mod(year, 4) == 0) then
                leapyear_year = .true.
            else
                leapyear_year = .false.
            endif
        end function

        !---------------------------------------------------------------------------------------------
        ! Returns number of days in a month of a year from a datenum
        !---------------------------------------------------------------------------------------------
        integer function monthdays_datenum(datenum)
            implicit none
            double precision, intent(in) :: datenum
            integer :: year, month
        
            call datevec(datenum, year, month)
            monthdays_datenum = monthdays_year_month(year, month)
        end function

        !------------------------------------------------------------------------------------------------
        ! Returns number of days in a month of a year from a year and a month
        !-----------------------------------------------------------------------------------------------
        integer function monthdays_year_month(year, month)
            implicit none
            integer, intent(in) :: year, month
            integer :: default_monthdays(12) = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
            integer :: iyear, imonth
            
            iyear = year
            imonth = month
            
            if(month>12) then
            	imonth = imonth-12
            	iyear = iyear + 1
            endif
                    
            if(leapyear(iyear) .and. imonth==2) then
                monthdays_year_month = 29
            else
                monthdays_year_month = default_monthdays(imonth)
            endif
        end function

        !------------------------------------------------------------------------------------------------
        ! Conver a date into string like YYYYMMDD from a year, month and day.
        !------------------------------------------------------------------------------------------------
        subroutine datestr_year_month_day(year,month, day, strdate)
            implicit none
            character(*),     intent(inout) :: strdate 
            integer,          intent(in)    :: year, month, day
        
            if(len(strdate)<8) then
                strdate = 'E'
                return;
            endif

            write(UNIT=strdate, FMT=9000) year, month, day

            9000 FORMAT (I4,2(I2.2))
        end subroutine


        !----------------------------------------------------------------------------------------------
        ! Conver a date into string like YYYYMMDD from a datenum.
        !----------------------------------------------------------------------------------------------
        subroutine datestr_datenum(datenum, strdate)
            implicit none
            character(*),      intent(inout) :: strdate 
            double precision, intent(in)    :: datenum
            integer :: year, month, day  
        
            if(len(strdate)<8) then
                return;
            endif

            call datevec(datenum, year, month, day)
            call datestr_year_month_day(year,month, day, strdate)
        end subroutine

    end module DateTime