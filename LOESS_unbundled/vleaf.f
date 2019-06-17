      subroutine ehg137(z,kappa,leaf,nleaf,d,nv,nvmax,ncmax,vc,a,xi,lo,h
     $i,c,v)
      integer d,execnt,nc,ncmax,nleaf,p,stackt
      integer a(ncmax),hi(ncmax),leaf(256),lo(ncmax),pstack(20)
      real xi(ncmax),z(d)
      external ehg182
      save execnt
      data execnt /0/
c     stacktop -> stackt
      execnt=execnt+1
c     find leaf cells affected by $z$
      stackt=0
      p=1
      nleaf=0
c     top of while loop
    3 if(.not.(0.lt.p))goto 4
         if(a(p).eq.0)then
c           leaf
            nleaf=nleaf+1
            leaf(nleaf)=p
c           Pop
            if(stackt.ge.1)then
               p=pstack(stackt)
            else
               p=0
            end if
            stackt=max(0,stackt-1)
         else
            if(z(a(p)).eq.xi(p))then
c              Push
               stackt=stackt+1
               if(.not.(stackt.le.20))then
                  call ehg182(187)
               end if
               pstack(stackt)=hi(p)
               p=lo(p)
            else
               if(z(a(p)).le.xi(p))then
                  p=lo(p)
               else
                  p=hi(p)
               end if
            end if
         end if
         goto 3
c     bottom of while loop
    4 if(.not.(nleaf.le.256))then
         call ehg182(185)
      end if
      return
      end
