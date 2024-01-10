program donut
  implicit none
  real :: A = 1.0
  real :: B = 2.0

  do  
    call system("clear")
    call render_frame(A, B)
    ! call sleep(1)
    A = A + 0.01
    B = B + 0.04
  end do
contains
  subroutine render_frame(A,B)
    implicit none
    integer, parameter :: SCREEN_WIDTH = 45
    integer, parameter :: SCREEN_HEIGHT = 105  
    real :: PI = 4.0 * atan(1.0)
    real :: THETA_SPACING = 0.08
    real :: PHI_SPACING = 0.01

    real :: theta, costheta, sintheta
    real :: phi, cosphi, sinphi
    real :: circlex, circley, x, y, z, az
    real :: lum, lumA, lumB, lumC
    integer :: xp, yp, lum_idx
    
    real, intent(in) :: A 
    real, intent(in) :: B
    real :: cosA, sinA, cosB, sinB
    real :: R1, R2, K2
    real :: K1

    integer, dimension(SCREEN_WIDTH,SCREEN_HEIGHT) :: zbuffer
    character, dimension(SCREEN_WIDTH,SCREEN_HEIGHT) :: output 
    
    integer :: characters(12)

    integer :: i 
    integer :: j

    zbuffer = 0 
    output = " " 
    characters = [46, 44, 45, 126, 58, 59, 61, 33, 42, 35, 36, 64]
    
    R1 = 1 
    R2 = 2
    K2 = 120

    K1 = SCREEN_WIDTH * K2 * 3 / (8 * (R1 + R2))
    
    cosA = cos(A)
    cosB = cos(B)  
    sinA = sin(A)
    sinB = sin(B)

    do theta = 0.0, 2 * PI, THETA_SPACING
      costheta = cos(theta)
      sintheta = sin(theta)

      do phi = 0, 2 * PI, PHI_SPACING
        cosphi = cos(phi)
        sinphi = sin(phi)
        circlex = R2 + R1 * costheta
        circley = R1 * sintheta

        x = circlex * (cosB * cosphi + sinA *sinB * sinphi) - circley * cosA * sinB
        y = circlex * (sinB * cosphi - sinA * cosB * sinphi) + circley * cosA * cosB
        z = K2 + cosA * circlex * sinphi + circley * sinA   
        az = 1.0 / z

        xp = int(SCREEN_WIDTH / 2 + (K1 * x * az))
        yp = int(SCREEN_HEIGHT / 2 - (K1 * y * az))

        lumA = cosphi*costheta*sinB
        lumB = cosA * costheta * sinphi - sinA * sintheta
        lumC = cosB * (cosA * sintheta - costheta * sinA * sinphi)
        
        lum = lumA - lumB + lumC
       
        if (xp >= 1 .and. xp <= SCREEN_WIDTH .and. yp >= 1 .and. yp <= SCREEN_HEIGHT) then
          if (lum > 0) then
            if (az > zbuffer(xp, yp)) then
              zbuffer(xp, yp) = az            
              lum_idx = int(lum * 8) 
              if (lum_idx < size(characters)) then
                output(xp, yp) = achar(characters(lum_idx + 1))
              end if
            end if
          end if 
        end if    
      end do
    end do

    do i = 1, SCREEN_WIDTH, 1
      do j = 1, SCREEN_HEIGHT, 1
        write(*, '(A)', advance='no') output(i, j)
      end do
      print *, "" 
    end do
  end subroutine render_frame
end program donut
