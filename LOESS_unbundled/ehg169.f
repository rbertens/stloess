      subroutine ehg169(d,vc,nc,ncmax,nv,nvmax,v,a,xi,c,hi,lo)
      integer d,execnt,i,j,k,mc,mv,nc,ncmax,nv,nvmax,p,vc
      integer a(ncmax),c(vc,ncmax),hi(ncmax),lo(ncmax),novhit(1)
      real v(nvmax,d),xi(ncmax)
      external ehg125,ehg182
      integer ifloor
      external ifloor
      save execnt
      data execnt /0/
      execnt=execnt+1
c     as in bbox
c     remaining vertices
      do 3 i=2,vc-1
         j=i-1
         do 4 k=1,d
            v(i,k)=v(1+mod(j,2)*(vc-1),k)
            j=ifloor(float(j)/2.)
    4    continue
    3 continue
c     as in ehg131
      mc=1
      mv=vc
      novhit(1)=-1
      do 5 j=1,vc
         c(j,mc)=j
    5 continue
c     as in rbuild
      p=1
c     top of while loop
    6 if(.not.(p.le.nc))goto 7
         if(a(p).ne.0)then
            k=a(p)
c           left son
            mc=mc+1
            lo(p)=mc
c           right son
            mc=mc+1
            hi(p)=mc
            call ehg125(p,mv,v,novhit,nvmax,d,k,xi(p),2**(k-1),2**(d-k),
     $c(1,p),c(1,lo(p)),c(1,hi(p)))
         end if
         p=p+1
         goto 6
c     bottom of while loop
    7 if(.not.(mc.eq.nc))then
         call ehg182(193)
      end if
      if(.not.(mv.eq.nv))then
         call ehg182(193)
      end if
      return
      end
