program energy_level

    implicit none

    double precision :: y1, y2, x, h, a, b, pi, p
    double precision :: a1, a2
    integer :: i, j, n

    y1(x) = cos(x) + p*(sin(x)/x) - 1
    y2(x) = cos(x) + p*(sin(x)/x) + 1

    pi = 3.141592653589793
    p = (3.0*pi)/2.0
    print *, pi

    print *, "enter n:"
    read *, n

    a = 1e-5
    b = 16.0

    h = (b-a)/float(n)

    do i = 0, n
        a1 = a + i*h
        a2 = a + (i+1)*h

        if (y1(a1)*y1(a2) < 0.0) then

            do j = 1, 30
                x = (a1+a2)/2.0
                print *, j, x , y1(x)

                if(abs(y1(x)) < 1e-7) exit

                if (y1(x)*y1(a1) < 0.0) then
                    a1 = a1
                    a2 = x

                else if (y1(x)*y1(a2) < 0.0) then
                    a1 = x
                    a2 = a2
                
                end if
                
            end do

            a1 = a + i*h
            a2 = a + (i+1)*h
            print *, a1, a2, x, y1(x), "upper cut"
            print *, "  "

        else if (y2(a1)*y2(a2) < 0.0) then

            do j = 1, 30
                x = (a1+a2)/2.0
                print *, j, x , y2(x)

                if(abs(y2(x)) < 1e-7) exit

                if (y2(x)*y2(a1) < 0.0) then
                    a1 = a1
                    a2 = x

                else if (y2(x)*y2(a2) < 0.0) then
                    a1 = x
                    a2 = a2
                
                end if
                
            end do

            a1 = a + i*h
            a2 = a + (i+1)*h
            print *, a1, a2, x, y2(x), "lower cut"
            print *, "  "

        end if

    end do

end program