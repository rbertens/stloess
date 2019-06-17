      integer d,i,tdeg,k,liv,lv,n,nvmax,tau0,nf,livm,ivm,nm
      parameter (nm=1000)
      parameter (livm=10000)
      parameter (ivm=20000)
      integer iwork(livm)
      real alpha,fcell
      real s(nm),w(nm),work(ivm),xx(nm,8),yy(nm)
      integer ifloor
      external ifloor
      read(5,*)n,d,alpha,tdeg,fcell
      nf=min(n,ifloor(n*alpha))
      nvmax=max(200,n)
      tau0=d+1
      if(tdeg.eq.2) tau0=((d+2)*(d+1))/2
      liv=50+(2**d+4)*nvmax+2*n
      lv =50+(3*d+3)*nvmax+n+(tau0+2)*nf
      if(nm.lt.n .or. livm.lt.liv .or. ivm.lt.lv)then
         print *, 'sample program needs more workspace'
      else
         call getxy(n,d,xx,yy,w)
         call lowesd(106,iwork,liv,lv,work,d,n,alpha,
     $       tdeg,nvmax,.false.)
         work(2)=fcell
         call lowesb(xx,yy,w,xx,.false.,iwork,liv,lv,work)
         call lowese(iwork,liv,lv,work,n,xx,s)
         do 4 i=1,n
            print *, s(i)
    4    continue
      end if
      end
