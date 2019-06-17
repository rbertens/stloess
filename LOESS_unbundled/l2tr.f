      subroutine ehg139(v,nvmax,nv,n,d,nf,f,x,pi,psi,y,rw,trl,kernel,k,d
     $ist,phi,eta,b,od,w,diagl,vval2,ncmax,vc,a,xi,lo,hi,c,vhit,rcond,si
     $ng,dd,tdeg,cdeg,lq,lf,setlf,s)
      logical setlf
      integer identi,d,dd,execnt,i,i2,i3,i5,i6,ii,ileaf,info,j,k,kernel,
     $l,n,nc,ncmax,nf,nleaf,nv,nvmax,od,sing,tdeg,vc
      integer lq(nvmax,nf),a(ncmax),c(vc,ncmax),cdeg(8),hi(ncmax),leaf(2
     $56),lo(ncmax),phi(n),pi(n),psi(n),vhit(nvmax)
      real f,i1,i4,i7,rcond,scale,term,tol,trl
      real lf(0:d,nvmax,nf),sigma(15),u(15,15),e(15,15),b(nf,k),diagl(n)
     $,dist(n),eta(nf),gamma(15),q(8),qraux(15),rw(n),s(0:od,nv),v(nvmax
     $,d),vval2(0:d,nv),w(nf),work(15),x(n,d),xi(ncmax),y(n),z(8)
      external ehg127,ehg182,sqrsl,ehg137
      real ehg128
      external ehg128
      real sdot
      external sdot
      save execnt
      data execnt /0/
c     V -> e
c     Identity -> identi
c     X -> b
      execnt=execnt+1
c     l2fit with trace(L)
      if(.not.(k.le.nf-1))then
         call ehg182(104)
      end if
      if(.not.(k.le.15))then
         call ehg182(105)
      end if
      if(trl.ne.0)then
         do 3 i5=1,n
            diagl(i5)=0
    3    continue
         do 4 i6=1,nv
            do 5 i5=0,d
               vval2(i5,i6)=0
    5       continue
    4    continue
      end if
      do 6 identi=1,n
         psi(identi)=identi
    6 continue
      do 7 l=1,nv
         do 8 i5=1,d
            q(i5)=v(l,i5)
    8    continue
         call ehg127(q,n,d,nf,f,x,psi,y,rw,kernel,k,dist,eta,b,od,w,rcon
     $d,sing,sigma,u,e,gamma,qraux,work,tol,dd,tdeg,cdeg,s(0,l))
         if(trl.ne.0)then
c           invert $psi$
            do 9 i5=1,n
               phi(i5)=0
    9       continue
            do 10 i=1,nf
               phi(psi(i))=i
   10       continue
            do 11 i5=1,d
               z(i5)=v(l,i5)
   11       continue
            call ehg137(z,vhit(l),leaf,nleaf,d,nv,nvmax,ncmax,vc,a,xi,lo
     $,hi,c,v)
            do 12 ileaf=1,nleaf
               do 13 ii=lo(leaf(ileaf)),hi(leaf(ileaf))
                  i=phi(pi(ii))
                  if(i.ne.0)then
                     if(.not.(psi(i).eq.pi(ii)))then
                        call ehg182(194)
                     end if
                     do 14 i5=1,nf
                        eta(i5)=0
   14                continue
                     eta(i)=w(i)
c                    $eta = Q sup T W e sub i$
                     call sqrsl(b,nf,nf,k,qraux,eta,work,eta,eta,work,wo
     $rk,1000,info)
                     do 15 j=1,k
                        if(tol.lt.sigma(j))then
                           i4=sdot(k,u(1,j),1,eta,1)/sigma(j)
                        else
                           i4=0.
                        end if
                        gamma(j)=i4
   15                continue
                     do 16 j=1,d+1
c                       bug fix 2006-07-15 for k=1, od>1.   (thanks btyner@gmail.com)
                        if(j.le.k)then
                           vval2(j-1,l)=sdot(k,e(j,1),15,gamma,1)
                        else
                           vval2(j-1,l)=0
                        end if
   16                continue
                     do 17 i5=1,d
                        z(i5)=x(pi(ii),i5)
   17                continue
                     term=ehg128(z,d,ncmax,vc,a,xi,lo,hi,c,v,nvmax,vval2
     $)
                     diagl(pi(ii))=diagl(pi(ii))+term
                     do 18 i5=0,d
                        vval2(i5,l)=0
   18                continue
                  end if
   13          continue
   12       continue
         end if
         if(setlf)then
c           $Lf sub {:,l,:} = V SIGMA sup {+} U sup T Q sup T W$
            if(.not.(k.ge.d+1))then
               call ehg182(196)
            end if
            do 19 i5=1,nf
               lq(l,i5)=psi(i5)
   19       continue
            do 20 i6=1,nf
               do 21 i5=0,d
                  lf(i5,l,i6)=0
   21          continue
   20       continue
            do 22 j=1,k
               do 23 i5=1,nf
                  eta(i5)=0
   23          continue
               do 24 i5=1,k
                  eta(i5)=u(i5,j)
   24          continue
               call sqrsl(b,nf,nf,k,qraux,eta,eta,work,work,work,work,10
     $000,info)
               if(tol.lt.sigma(j))then
                  scale=1./sigma(j)
               else
                  scale=0.
               end if
               do 25 i5=1,nf
                  eta(i5)=eta(i5)*(scale*w(i5))
   25          continue
               do 26 i=1,nf
                  i7=eta(i)
                  do 27 i5=0,d
                     if(i5.lt.k)then
                        lf(i5,l,i)=lf(i5,l,i)+e(1+i5,j)*i7
                     else
                        lf(i5,l,i)=0
                     end if
   27             continue
   26          continue
   22       continue
         end if
    7 continue
      if(trl.ne.0)then
         if(n.le.0)then
            trl=0.
         else
            i3=n
            i1=diagl(i3)
            do 28 i2=i3-1,1,-1
               i1=diagl(i2)+i1
   28       continue
            trl=i1
         end if
      end if
      return
      end
