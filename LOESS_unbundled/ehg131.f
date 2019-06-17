      subroutine ehg131(x,y,rw,trl,diagl,kernel,k,n,d,nc,ncmax,vc,nv,nvm
     $ax,nf,f,a,c,hi,lo,pi,psi,v,vhit,vval,xi,dist,eta,b,ntol,fd,w,vval2
     $,rcond,sing,dd,tdeg,cdeg,lq,lf,setlf)
      logical setlf
      integer identi,d,dd,execnt,i1,i2,j,k,kernel,n,nc,ncmax,nf,ntol,nv,
     $nvmax,sing,tdeg,vc
      integer lq(nvmax,nf),a(ncmax),c(vc,ncmax),cdeg(8),hi(ncmax),lo(ncm
     $ax),pi(n),psi(n),vhit(nvmax)
      real f,fd,rcond,trl
      real lf(0:d,nvmax,nf),b(*),delta(8),diagl(n),dist(n),eta(nf),rw(n)
     $,v(nvmax,d),vval(0:d,nvmax),vval2(0:d,nvmax),w(nf),x(n,d),xi(ncmax
     $),y(n)
      external ehg126,ehg182,ehg139,ehg124
      real snrm2
      external snrm2
      save execnt
      data execnt /0/
c     Identity -> identi
c     X -> b
      execnt=execnt+1
      if(.not.(d.le.8))then
         call ehg182(101)
      end if
c     build $k$-d tree
      call ehg126(d,n,vc,x,v,nvmax)
      nv=vc
      nc=1
      do 3 j=1,vc
         c(j,nc)=j
         vhit(j)=0
    3 continue
      do 4 i1=1,d
         delta(i1)=v(vc,i1)-v(1,i1)
    4 continue
      fd=fd*snrm2(d,delta,1)
      do 5 identi=1,n
         pi(identi)=identi
    5 continue
      call ehg124(1,n,d,n,nv,nc,ncmax,vc,x,pi,a,xi,lo,hi,c,v,vhit,nvmax,
     $ntol,fd,dd)
c     smooth
      if(trl.ne.0)then
         do 6 i2=1,nv
            do 7 i1=0,d
               vval2(i1,i2)=0
    7       continue
    6    continue
      end if
      call ehg139(v,nvmax,nv,n,d,nf,f,x,pi,psi,y,rw,trl,kernel,k,dist,di
     $st,eta,b,d,w,diagl,vval2,nc,vc,a,xi,lo,hi,c,vhit,rcond,sing,dd,tde
     $g,cdeg,lq,lf,setlf,vval)
      return
      end
