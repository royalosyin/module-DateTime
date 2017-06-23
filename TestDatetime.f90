subroutine testDatetime()
    use DateTime
        
    implicit none
    character*8  date
    character*10 time
    character*5  zone    
    character*8 strDate

    double precision thisdaynum
    integer*4    values(8)
    integer year, month, day    

    !------------------ Get current date a time----------------------------
    call date_and_time( date , time, zone, values )
    
    !------------------Test convter into data number----------------------
    call datenum(thisdaynum, values(1), values(2), values(3))
    write(*,*) 'Today is :', date
    write(*,*) 'The date serial number is: ', thisdaynum

    !------------------Test date number addition---------------------------
    call addtodate(thisdaynum, -1, 'day')
    call datevec(thisdaynum, year, month, day)

    call addtodate(thisdaynum, 17, 'day')
    call datevec(thisdaynum, year, month, day)


    
    !-----------------Test convert a date number into string date ---------
    call datestr(thisdaynum, strDate)
    write(*,*) '----------------------------------------------------------'
    write(*,*) 'The day after 17 days will be:  ',  strDate  
    write(*,*) 'New date serial number is: ',       thisdaynum   


end subroutine