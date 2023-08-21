# Twobodyproblem2
 R with respect to theta
 program orbitalmotion

        implicit none

        real :: G 
        real :: m1, m2, A, j, m, h, c, r, u, r0, u0, vel_0, vel
        real :: k, k1, k2, k3, k4, l1, l2, l3, l4, z0 !declaring real numbers
        integer :: t
        m1 = 1000!mass of first body
        m2 = 10!mass of second body
        j = 15!angular momentum of a body
        h = 0.1
        G = 6.67!gravitational constant
         !initial condiation
        r0 = 8
        vel_0 =20

        u0 = (1/r0)
        m = (m1*m2)/(m1+m2)!reduced mass
        A = (j**2)/m!declaring constant
        k = G*m1*m2!declaring constant
        c = k/A!declaring constant
        z0 = vel_0

         !RK method
        do t = 1,62
        k1 = h*z0
        l1 = h*(c-u0)
        k2 = h*(z0+(l1/2))
        l2 = h*(c-(u0+(k1/2)))
        k3 = h*(z0+(l2/2))
        l3 = h*(c-(u0+(k2/2)))
        k4 = h*(z0+l3)
        l4 = h*(c-(u0+k3))
        u = u0+((k1+(2*k2)+(2*k3)+k4)/6)
        r = (1/u)
        !vel = vel_0+((1/6)*(l1+(2*l2)+(2*l3)+l4))
        print*, r
        write(12,*)t,r
        u0=u
        !vel_0=vel
       
        end do
        end program orbitalmotion

