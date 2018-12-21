import basetypes
import geomutils
import cgradient
import math


class TRMSDCalculator:
    # FFixed, FMobile, FPositions, FDerivs = TCoords, FRotation, FTranslation, FCenterVec = TCoord,
    # FBaseRotation = TRotMatrix, FCGMinimizer = TCGMinimizer, FRmsd = TFloat
    def __init__(self):
        self.FBaseRotation = geomutils.BuildRotation(basetypes.TCoord(0, 0, 0), geomutils.MCXYZRotation)
        self.FCGMinimizer = cgradient.TCGMinimiser()
        self.FCGMinimizer.FGetValue = self.Evaluate
        self.FCGMinimizer.FGetValueAndDerivative = self.EvaluateAndDerive
        self.FFixed = []
        self.FMobile = []
        self.FPositions = []
        self.FDerivs = []
        self.FRotation = basetypes.TCoord()
        self.FTranslation = basetypes.TCoord()
        self.FCenterVec = basetypes.TCoord()
        self.FRmsd = 0

    # Point, Deriv = TFloats
    def EvaluateAndDerive(self, Point, Deriv):
        Result = 0
        for f in range(3):
            self.FRotation[f] = Point[f]
            Point[f] = 0
            self.FTranslation[f] = Point[f + 3]
        RotMatrix = self.CurrentRotation()
        for f in range(len(self.FMobile)):
            Coords = geomutils.RotAndPlace(RotMatrix, self.FTranslation, self.FMobile[f])
            Dist = (Coords[0] - self.FFixed[f][0]) ** 2 + (Coords[1] - self.FFixed[f][1]) ** 2 + \
                   (Coords[2] - self.FFixed[f][2]) ** 2
            Result = Result + Dist
            self.FDerivs[f] = geomutils.Subtract(self.FFixed[f], Coords)
            self.FPositions[f] = Coords
        self.GetDerivatives(Deriv)
        # Return TFloat
        return Result

    # Point = const TFloats
    def Evaluate(self, Point):
        Result = 0
        for f in range(3):
            self.FRotation[f] = Point[f]
            self.FTranslation[f] = Point[f + 3]
        RotMatrix = self.CurrentRotation()
        for f in range(len(self.FMobile)):
            c = geomutils.RotAndPlace(RotMatrix, self.FTranslation, self.FMobile[f])
            Result = Result + (c[0] - self.FFixed[f][0]) ** 2 + (c[1] - self.FFixed[f][1]) ** 2 + \
                     (c[2] - self.FFixed[f][2]) ** 2
            self.FPositions[f] = c
        # Return TFloat
        return Result

    # Deriv = TFloats
    def GetDerivatives(self, Deriv):
        XVector = basetypes.TCoord(1, 0, 0)
        YVector = basetypes.TCoord(0, 1, 0)
        ZVector = basetypes.TCoord(0, 0, 1)

        self.FBaseRotation = self.CurrentRotation()
        self.FRotation = basetypes.TCoord()
        TDeriv = self.Shear()
        Pivot = self.FTranslation
        Axis = XVector
        RDeriv = basetypes.TCoord()
        RDeriv[0] = self.Torsion(Pivot, Axis)
        Axis = YVector
        RDeriv[1] = self.Torsion(Pivot, Axis)
        Axis = ZVector
        RDeriv[2] = self.Torsion(Pivot, Axis)
        for f in range(3):
            Deriv[f] = RDeriv[f]
            Deriv[f + 3] = TDeriv[f]

    # Pivot, Axis = TCoord
    def Torsion(self, Pivot, Axis):
        Result = 0
        for f in range(len(self.FDerivs)):
            AtDeriv = self.FDerivs[f]
            if (AtDeriv[0] is not 0) or (AtDeriv[1] is not 0) or (AtDeriv[2] is not 0):
                v1 = geomutils.Subtract(self.FPositions[f], Pivot)
                v1 = geomutils.CrossProduct(Axis, v1)
                Result = Result + geomutils.DotProduct(v1, AtDeriv)
        # Return TFloat
        return Result

    def Shear(self):
        Result = basetypes.TCoord()
        for f in range(len(self.FFixed)):
            Result = geomutils.Add(self.FDerivs[f], Result)
        # Return TCoord
        return Result

    def CurrentRotation(self):
        # Return TRotMatrix
        return geomutils.Multiply(geomutils.BuildRotation(self.FRotation, geomutils.MCXYZRotation), self.FBaseRotation)

    def Initialize(self):
        self.FMobile = []
        self.FFixed = []
        self.FDerivs = []

    # Fixed, Mobile = TCoord OR TCoords
    def AddCoordinates(self, Fixed, Mobile):
        if isinstance(Fixed, basetypes.TCoord):
            self.FFixed.append(Fixed)
            self.FMobile.append(Mobile)
        else:
            for f in range(len(Fixed)):
                self.FFixed.append(Fixed[f])
                self.FMobile.append(Mobile[f])

    # MaxIterations = Integer, MinVariation = TFloat
    def Minimise(self, MaxIterations, MinVariation):
        self.Center()
        self.FDerivs = []
        self.FPositions = []
        InitialPoint = []
        InitialDeriv = []
        for f in range(6):
            InitialPoint.append(0)
        self.FirstTranslation(InitialPoint)
        self.EvaluateAndDerive(InitialPoint, InitialDeriv)
        Result = self.FCGMinimizer.Minimize(MaxIterations, MinVariation, InitialPoint, InitialDeriv)
        if len(self.FMobile) > 0:
            Result = math.sqrt(Result / len(self.FMobile))
        self.FRmsd = Result
        self.FDerivs = []
        self.FPositions = []
        self.FBaseRotation = self.CurrentRotation()
        self.FRotation = basetypes.TCoord()
        # Return Integer
        return Result

    # InitialPoint = TFloats
    def FirstTranslation(self, InitialPoint):
        FF = basetypes.TCoord()
        for f in range(len(self.FFixed)):
            FF = geomutils.Add(FF, self.FFixed[f])
        if len(self.FFixed) > 0:
            FF = geomutils.Multiply(FF, 1 / len(self.FFixed))
            for f in range(3):
                InitialPoint[f + 3] = FF[f]

    def Center(self):
        self.FCenterVec = basetypes.TCoord()
        for f in range(len(self.FMobile)):
            self.FCenterVec = geomutils.Add(self.FCenterVec, self.FMobile[f])
        if len(self.FMobile) > 0:
            self.FCenterVec = geomutils.Multiply(self.FCenterVec, 1 / len(self.FMobile))
        for f in range(len(self.FMobile)):
            self.FMobile[f] = geomutils.Subtract(self.FMobile[f], self.FCenterVec)

    # Fixed, Mobile = TCoords
    def SetCoords(self, Fixed, Mobile):
        self.FFixed = []
        for f in range(len(Fixed)):
            self.FFixed.append(Fixed[f])
        self.FMobile = []
        for f in range(Mobile):
            self.FMobile.append(Mobile[f])

    def BuildPositions(self):
        RotMatrix = self.CurrentRotation()
        for f in range(len(self.FMobile)):
            self.FPositions.append(geomutils.RotAndPlace(RotMatrix, self.FTranslation, self.FMobile[f]))

    # Index = Integer
    def GetCoord(self, Index):
        # Return TCoord
        return self.FPositions[Index]

    # Coords = const TCoords
    def TransformedCoords(self, Coords):
        Result = []
        for f in range(len(Coords)):
            Result.append(geomutils.Subtract(Coords[f], self.FCenterVec))
            Result[f] = geomutils.RotAndPlace(self.FBaseRotation, self.FTranslation, Result[f])
        # Return TCoords
        return Result
