program donut
  implicit none
  real :: A = 1.5
  real :: B = 1.5

    call render_frame(A, B)
  do  
    call system("clear")
    call render_frame(A, B)
    A = A + 0.01
    B = B + 0.04
  end do
contains
  subroutine render_frame(A,B)
    implicit none
    integer, parameter :: SCREEN_WIDTH = 50
    integer, parameter :: SCREEN_HEIGHT = 100
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

    character, dimension(SCREEN_WIDTH,SCREEN_HEIGHT) :: output 
    integer, dimension(SCREEN_WIDTH,SCREEN_HEIGHT) :: zbuffer = 0
    integer, dimension(SCREEN_WIDTH,SCREEN_HEIGHT) :: shades = 0
    integer :: colors(12)
    character(len=12) :: characters = ".,-~:;=!*#$@"

    integer :: i 
    integer :: j
    character(len=10) :: color

    output = " "
    colors = [53, 54, 55, 56, 57, 90, 91, 92, 93, 127, 128, 129]

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
              if(lum_idx >= 0 .and. lum_idx < 12) then
                output(xp, yp) = characters(lum_idx+1:lum_idx+1)
                shades(xp, yp) = colors(lum_idx + 1) 
              end if
            end if
          end if 
        end if    
      end do
    end do

    do i = 1, SCREEN_WIDTH, 1
      do j = 1, SCREEN_HEIGHT, 1
        write(color, '(I0)') shades(i,j)
        write(*,'(A)',advance='no') achar(27)//'[38;5;'//trim(color)//'m'//output(i,j)//achar(27)//'[0m'
      end do
      print *, "" 
    end do
  end subroutine render_frame
end program donut
