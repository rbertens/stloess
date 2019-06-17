      subroutine ehg140(iw,i,j)
      integer execnt,i,j
      integer iw(i)
      save execnt
      data execnt /0/
      execnt=execnt+1
      iw(i)=j
      return
      end
