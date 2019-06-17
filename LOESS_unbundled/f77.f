      integer function ifloor(x)
      real x
      ifloor=x
      if(ifloor.gt.x) ifloor=ifloor-1
      end
      real function sign(a1,a2)
      real a1, a2
      sign=abs(a1)
      if(a2.ge.0) sign=-sign
      end
