      subroutine ehg133(n,d,vc,nvmax,nc,ncmax,a,c,hi,lo,v,vval,xi,m,z,s)
      integer d,execnt,i,i1,m,nc,ncmax,nv,nvmax,vc
      integer a(ncmax),c(vc,ncmax),hi(ncmax),lo(ncmax)
      real delta(8),s(m),v(nvmax,d),vval(0:d,nvmax),xi(ncmax),z(m,d)
      real ehg128
      external ehg128
      save execnt
      data execnt /0/
      execnt=execnt+1
      do 3 i=1,m
         do 4 i1=1,d
            delta(i1)=z(i,i1)
    4    continue
         s(i)=ehg128(delta,d,ncmax,vc,a,xi,lo,hi,c,v,nvmax,vval)
    3 continue
      return
      end
