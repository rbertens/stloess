      subroutine lohead(iunit,d,vc,nc,nv)
      integer iunit,d,vc,nc,nv
      read(iunit,*)d,nc,nv
      vc = 2**d
      end
