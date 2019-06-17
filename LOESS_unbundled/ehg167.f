      subroutine ehg167(iunit,d,vc,nc,nv,nvmax,v,a,xi,vval)
      integer iunit,d,vc,nc,nv,a(nc),magic,i,j
      real v(nvmax,d),xi(nc),vval(0:d,nv)
      write(iunit,*)d,nc,nv
      do 10 i=1,d
10      write(iunit,*)v(1,i),v(vc,i)
      j = 0
      do 20 i=1,nc
        if(a(i).ne.0)then
          write(iunit,*)a(i),xi(i)
        else
          write(iunit,*)a(i),j
        end if
20    continue
      do 30 i=1,nv
30      write(iunit,*)(vval(j,i),j=0,d)
      end
