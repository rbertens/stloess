      subroutine ehg168(iunit,d,vc,nc,nv,nvmax,v,a,xi,vval)
      integer iunit,d,vc,nc,nv,a(nc),magic,i,j
      real v(nvmax,d),xi(nc),vval(0:d,nv)
      do 10 i=1,d
10      read(iunit,*)v(1,i),v(vc,i)
      do 20 i=1,nc
20      read(iunit,*)a(i),xi(i)
      do 30 i=1,nv
30      read(iunit,*)(vval(j,i),j=0,d)
      end
