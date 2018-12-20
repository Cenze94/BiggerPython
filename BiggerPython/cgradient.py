import basetypes
import geomutils


class TCGMinimiser:
    # Mins, Maxs = TFloats, Precision = TFloat
    # FGetValue = TCGGetValue = function(const Point:TFloats):TFloat of object
    # FGetValueAndDerivative = TCGGetValueAndDerivative = function(var Point:TFloats; var Deriv:TFloats):TFloat of
    # object
    # FReport = TCGReport = procedure(Iteration:Integer; Value:TFloat) of object
    def __init__(self, Mins = None, Maxs = None, Precision = 1e-8):
        self.FGetValue = None
        self.FGetValueAndDerivative = None
        self.FReport = None
        self.FLineCounts = -1
        self.FMins = []
        if Mins is not None:
            for f in range(len(Mins)):
                self.FMins.append(Mins[f])
        self.FMaxs = []
        if Maxs is not None:
            for f in range(len(Maxs)):
                self.FMaxs.append(Maxs[f])
        self.FPrecision = Precision

    # MaxIterations = Integer, MinVariation = TFloat, Point, Deriv = TFloats
    def Minimize(self, MaxIterations, MinVariation, Point, Deriv):
        g = []
        h = []
        Result = self.FGetValue(Point)
        for f in range(len(Point)):
            g.append(-Deriv[f])
            Deriv[f] = g[f]
            h.append(g[f])
        for f in range(1, MaxIterations):
            oldr = Result
            self.LineMinimization(Point, Deriv)
            Result = self.FGetValueAndDerivative(Point, Deriv)
            if self.FReport is not None:
                self.FReport(f, Result)
            dgg = 0
            dg = 0
            for j in range(len(Point)):
                dg = dg + (g[j]) ** 2
                dgg = dgg + (Deriv[j] + g[j]) * Deriv[j]
            if (dg == 0) or (oldr - Result <= MinVariation):
                # Return TFloat (and TFloats, TFloats in Python)
                return Result, Point, Deriv
            else:
                gam = dgg / dg
                for j in range(len(Point)):
                    g[j] = -Deriv[j]
                    Deriv[j] = g[j] + gam * h[j]
                    h[j] = Deriv[j]
        # Return TFloat (and TFloats, TFloats in Python)
        return Result, Point, Deriv

    # Point = TFloats, Direction = const TFloats
    def LineMinimization(self, Point, Direction):
        LMin, LMax = self.GetLineLimits(Point, Direction)
        CurrentPoint = []
        ax = 0
        fa = self.Evaluate(ax, LMin, LMax, Point, Direction, CurrentPoint)
        xmin = ax
        bx = self.FPrecision
        while True:
            fb = self.Evaluate(bx, LMin, LMax, Point, Direction, CurrentPoint)
            if (fb == fa) and (bx < self.FPrecision * 1e4):
                bx = bx * 10
            if (fb is not fa) or (bx >= self.FPrecision*1e4):
                break
        b, ax, bx, cx, fa, fb, fc = self.Bracket(ax, bx, fa, fb, LMin, LMax, Point, Direction, CurrentPoint)
        if b and (bx is not cx):
            xmin, fa = self.Brent(ax, bx, cx, LMin, LMax, Point, Direction, CurrentPoint)
        self.SetPoints(xmin, Point, Direction)
        # Return TFloat
        return fa

    # Point, Direction = TFloats
    def GetLineLimits(self, Point, Direction):
        if self.FMaxs is not None:
            LMin = 1e-30
            LMax = -1e-30
            for f in range(len(self.FMaxs)):
                if abs(Direction[f]) > 1e-30:
                    LMin = basetypes.Min(LMin, (self.FMins[f] - Point[f]) / Direction[f])
                    LMax = basetypes.Max(LMax, (self.FMaxs[f] - Point[f]) / Direction[f])
            # In Python return TFloat, TFloat
            return LMin, LMax
        # In Python return TFloat, TFloat
        return 0, 0

    # x = TFloat, FLineCounts = Integer, LMin LMax = TFloat, Point, Direction, CurrentPoint = TFloats
    def Evaluate(self, x, LMin, LMax, Point, Direction, CurrentPoint):
        if self.FMaxs is not None:
            if x > LMax:
                x = LMax
                Result = 1e30
            elif x < LMin:
                x = LMin
                Result = 1e30
            else:
                for f in range(len(Point)):
                    CurrentPoint.append(Point[f] + x * Direction[f])
                Result = self.FGetValue(CurrentPoint)
        else:
            for f in range(len(Point)):
                CurrentPoint.append(Point[f] + x * Direction[f])
            Result = self.FGetValue(CurrentPoint)
        # Return TFloat
        return Result

    # ax, bx, cx, fa, fb, fc, LMin, LMax = TFloat, Point, Direction, CurrentPoint = TFloats
    def Bracket(self, ax, bx, fa, fb, LMin, LMax, Point, Direction, CurrentPoint):
        GOLD = 1.61834
        TINY = 1.0e-20
        GLIMIT = 100.0
        if fb > fa:
            dum = ax
            ax = bx
            bx = dum
            dum = fa
            fa = fb
            fb = dum
        cx = bx + GOLD * (bx - ax)
        fc = self.Evaluate(cx, LMin, LMax, Point, Direction, CurrentPoint)
        while fb > fc:
            r = (bx - ax) * (fb - fc)
            q = (bx - cx) * (fb - fa)
            dum = 2 * abs(q - r)
            if dum < TINY:
                dum = TINY
            u = bx - (( bx - cx) * q - (bx - ax) * r) / dum
            ulim = bx + GLIMIT * (cx - bx)
            if (bx - u) * (u - cx) > 0:
                fu = self.Evaluate(u, LMin, LMax, Point, Direction, CurrentPoint)
                if fu < fc:
                    ax = bx
                    bx = u
                    fa = fb
                    fb = fu
                    break
                elif fu > fb:
                    cx = u
                    fc = fu
                    break
                u = cx + GOLD * (cx - bx)
                fu = self.Evaluate(u, LMin, LMax, Point, Direction, CurrentPoint)
            elif (cx - u) * (u - ulim) > 0:
                fu = self.Evaluate(u, LMin, LMax, Point, Direction, CurrentPoint)
                if fu < fc:
                    bx = cx
                    cx = u
                    u = cx + GOLD * (cx - bx)
                    fb = fc
                    fc = fu
                    fu = self.Evaluate(u, LMin, LMax, Point, Direction, CurrentPoint)
            elif (u - ulim) * (ulim - cx) >= 0:
                u = ulim
                fu = self.Evaluate(u, LMin, LMax, Point, Direction, CurrentPoint)
            else:
                u = cx + GOLD * (cx - bx)
                fu = self.Evaluate(u, LMin, LMax, Point, Direction, CurrentPoint)
            ax = bx
            bx = cx
            cx = u
            fa = fb
            fb = fc
            fc = fu
        # Return Boolean
        return True, ax, bx, cx, fa, fb, fc

    # ax, bx, cx, LMin, LMax = TFloat, Point, Direction, CurrentPoint = TFloats
    def Brent(self, ax, bx, cx, LMin, LMax, Point, Direction, CurrentPoint):
        ZEPS = 10e-10
        R = 0.6180399
        CR = 1 - R

        e = 0
        d = 0
        if ax < cx:
            a = ax
        else:
            a = cx
        if ax > cx:
            b = ax
        else:
            b = cx
        x = bx
        w = bx
        v = bx
        fw = bx
        fv = bx
        fx = bx
        for iter in range(1, 101):
            xm = 0.5 * (a + b)
            tol1 = self.FPrecision * abs(x) + ZEPS
            tol2 = 2.0 * tol1
            if abs(x - xm) < (tol2 - 0.5 * (b - a)):
                xmin = x
                break
            if abs(e) > tol1:
                r = (x - w) * (fx - fw)
                q = (x - v) * (fx - fv)
                p = (x - v) * q - (x - w) * r
                q = 2 * (q - r)
                if q > 0:
                    p = -p
                q = abs(q)
                etemp = e
                e = d
                if (abs(p) >= abs(0.5 * q * etemp)) or (p <= q * (a - x)) or (p >= q * (b - x)):
                    if x >= xm:
                        e = a - x
                    else:
                        e = b - x
                    d = CR * e
                else:
                    d = p / q
                    u = x + d
                    if (u - a < tol2) or (b - u < tol2):
                        if xm - x > 0:
                            d = tol1
                        else:
                            d = -tol1
            else:
                if x >= xm:
                    e = a - x
                else:
                    e = b - x
                d = CR * e
            if abs(d) > tol1:
                u = x + d
            else:
                if d > 0:
                    u = x + tol1
                else:
                    u = x - tol1
            fu = self.Evaluate(u, LMin, LMax, Point, Direction, CurrentPoint)
            if fu <= fx:
                if u >= x:
                    a = x
                else:
                    b = x
                v = w
                w = x
                x = u
                fv = fw
                fw = fx
                fx = fu
            else:
                if u < x:
                    a = u
                else:
                    b = u
                if (fu <= fw) or (w == x):
                    v = w
                    w = u
                    fv = fw
                    fw = fu
                elif (fu <= fv) or (v == x) or (v == w):
                    v = u
                    fv = fu
        # In Python return TFloat, TFloat
        return x, fx

    # x = TFloat, Point, Direction = TFloats
    def SetPoints(self, x, Point, Direction):
        for f in range(len(Point)):
            Point[f] = Point[f] + x * Direction[f]
