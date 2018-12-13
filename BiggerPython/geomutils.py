import basetypes
import numpy as np
import math


class TRotMatrix:
    def __init__(self, value11=None, value12=None, value13=None, value21=None, value22=None, value23=None,
                 value31=None, value32=None, value33=None):
        # TRotMatrix can ve initialized with 3 lists of elements or 9 values
        if isinstance(value11, list) and isinstance(value12, list) and isinstance(value13, list):
            self.matrix = np.array([value11, value12, value13])
        else:
            if value11 is None:
                value11 = 0
            if value12 is None:
                value12 = 0
            if value13 is None:
                value13 = 0
            if value21 is None:
                value21 = 0
            if value22 is None:
                value22 = 0
            if value23 is None:
                value23 = 0
            if value31 is None:
                value31 = 0
            if value32 is None:
                value32 = 0
            if value33 is None:
                value33 = 0
            value11 = float(value11)
            value12 = float(value12)
            value13 = float(value13)
            value21 = float(value21)
            value22 = float(value22)
            value23 = float(value23)
            value31 = float(value31)
            value32 = float(value32)
            value33 = float(value33)
            self.matrix = np.array([[value11, value12, value13], [value21, value22, value23], [value31, value32,
                                                                                               value33]])

    def __getitem__(self, pos):
        row, column = pos
        return self.matrix[row, column]

    def __setitem__(self, pos, value):
        row, column = pos
        self.matrix[row, column] = value


class TQuaternion:
    def __init__(self, value1 = None, value2 = None, value3 = None, value4 = None):
        self.values = []
        if value1 is None:
            value1 = 0
        if value2 is None:
            value2 = 0
        if value3 is None:
            value3 = 0
        if value4 is None:
            value4 = 0
        value1 = float(value1)
        value2 = float(value2)
        value3 = float(value3)
        value4 = float(value4)
        self.values.append(value1)
        self.values.append(value2)
        self.values.append(value3)
        self.values.append(value4)

    def __getitem__(self, item):
        return self.values[item]

    def __setitem__(self, key, value):
        self.values[key] = value


IdentityMatrix = TRotMatrix([1, 0, 0], [0, 1, 0], [0, 0, 1])
IdentityQuaternion = TQuaternion(1, 0, 0, 0)
PI = 3.1415926535897932

# Rotation types
MCXYZRotation = 0
MCRotationXYAxis = 1


# r, i, j, k = TFloat
def Quaternion(r, i, j, k):
    # Return TQuaternion
    return TQuaternion(r, i, j, k)


# First function: v1 = TCoord, v2 = TFloat
# Second function: v1, v2 = TCoord
# Third function: v1 = TCoord, v2 = TCoords
# Fourth function: v1 = TFloats, v2 = TFloat
# Fifth function: v1 = TCoords, v2 = TFloats
def Add(v1, v2):
    if isinstance(v1, basetypes.TCoord):
        if isinstance(v2, basetypes.TCoord):
            # Second function
            # Return TCoord
            return basetypes.TCoord(v1[0] + v2[0], v1[1] + v2[1], v1[2] + v2[2])
        elif isinstance(v2, list):
            # Third function
            Result = []
            for f in range(len(v2)):
                Result.append(Add(v1, v2[f]))
            # Return TCoords
            return Result
        else:
            # First function
            return basetypes.TCoord(v1[0] + v2, v1[1] + v2, v1[2] + v2)
    else:
        if isinstance(v2, list):
            # Fifth function
            Result = []
            for f in range(len(v2)):
                Result.append(Add(v1[f], v2[f]))
            # Return TCoords
            return Result
        else:
            # Fourth function
            Result = []
            for f in range(len(v1)):
                Result.append(v1[f] + v2)
            # Return TFloats
            return Result


# First function: v1 = TCoord, v2 = TFloat
# Second function: v1, v2 = TCoord
# Third function: v1 = TCoords, v2 = TCoord
def Subtract(v1, v2):
    if isinstance(v1, basetypes.TCoord):
        if isinstance(v2, basetypes.TCoord):
            # Second function
            # Return TCoord
            return basetypes.TCoord(v1[0] - v2[0], v1[1] - v2[1], v1[2] - v2[2])
        else:
            # First function
            # Return TCoord
            return basetypes.TCoord(v1[0] - v2, v1[1] - v2, v1[2] - v2)
    else:
        # Third function
        Result = []
        for f in range(len(v1)):
            Result.append(Subtract(v1[f], v2))
        # Return TCoords
        return Result


# First function: v1 = TCoord, v2 = TFloat
# Second function: v1 = TFloats, v2 = TFloat
# Third function: v1 = TCoords, v2 = TFloat
# Fourth function: v1, v2 = TQuaternion
# Fifth function: v1, v2 = TRotMatrix
def Multiply(v1, v2):
    if isinstance(v1, basetypes.TCoord):
        # First function
        # Return TCoord
        return basetypes.TCoord(v1[0] * v2, v1[1] * v2, v1[2] * v2)
    elif isinstance(v1, TQuaternion):
        # Fourth function
        # Return TQuaternion
        return TQuaternion(v1[0] * v2[0] - v1[1] * v2[1] - v1[2] * v2[2] - v1[3] * v2[3],
                           v1[0] * v2[1] + v1[1] * v2[0] + v1[2] * v2[3] - v1[3] * v2[2],
                           v1[0] * v2[2] - v1[1] * v2[3] + v1[2] * v2[0] + v1[3] * v2[1],
                           v1[0] * v2[3] + v1[1] * v2[2] - v1[2] * v2[1] + v1[3] * v2[0])
    elif isinstance(v1, TRotMatrix):
        # Fifth function
        Result = TRotMatrix()
        for f in range(3):
            for g in range(3):
                Result[f, g] = v1[f, 0] * v2[0, g] + v1[f, 1] * v2[1, g] + v1[f, 2] * v2[2, g]
        # Return TRotMatrix
        return Result
    elif isinstance(v1[0], basetypes.TCoord):
        # Third function
        Result = []
        for f in range(len(v1)):
            Result.append(Multiply(v1[f], v2))
        # Return TCoords
        return Result
    else:
        # Second function
        Result = []
        for f in range(len(v1)):
            Result.append(v1[f] * v2)
        # Return TFloats
        return Result


# v1, v2 = TCoord
def DotProduct(v1, v2):
    # Return TFloat
    return v1[0]*v2[0] + v1[1]*v2[1] + v1[2]*v2[2]


# v1, v2 = TCoord
def CrossProduct(v1, v2):
    Result = basetypes.TCoord()
    Result[0] = v1[1] * v2[2] - v1[2] * v2[1]
    Result[1] = v1[2] * v2[0] - v1[0] * v2[2]
    Result[2] = v1[0] * v2[1] - v1[1] * v2[0]
    # Return TCoord
    return Result


# Vec = TCoord
def Norm(Vec):
    # Return TFloat
    return math.sqrt(Vec[0] ** 2 + Vec[1] ** 2 + Vec[2] ** 2)


# Quat = const TQuaternion
def Conjugated(Quat):
    # Return TQuaternion
    return TQuaternion(Quat[0], - Quat[1], - Quat[2], - Quat[3])


# Vec = TCoord OR TQuaternion
def Normalize(Vec):
    if isinstance(Vec, basetypes.TCoord):
        n = math.sqrt(Vec[0] ** 2 + Vec[1] ** 2 + Vec[2] ** 2)
        Vec[0] = Vec[0] / n
        Vec[1] = Vec[1] / n
        Vec[2] = Vec[2] / n
    else:
        n = math.sqrt(Vec[0] ** 2 + Vec[1] ** 2 + Vec[2] ** 2 + Vec[3] ** 2)
        Vec[0] = Vec[0] / n
        Vec[1] = Vec[1] / n
        Vec[2] = Vec[2] / n
        Vec[3] = Vec[3] / n


# Vec = TCoord, v2 = TFloat
def Scaled(Vec, Scale):
    # Return TCoord
    return basetypes.TCoord(Vec[0] * Scale, Vec[1] * Scale, Vec[2] * Scale)


# Vec = TCoord OR TCoords
def Simmetric(Vec):
    if isinstance(Vec, basetypes.TCoord):
        # Return TCoord
        return basetypes.TCoord(- Vec[0], - Vec[1], - Vec[2])
    else:
        Result = []
        for f in range(len(Vec)):
            Result.append(Simmetric(Vec[f]))
        # Return TCoords
        return Result


# Vec1, Vec2 = TCoord OR TCoords OR TQuaternion
def Distance(Vec1, Vec2):
    if isinstance(Vec1, basetypes.TCoord):
        # Return TFloat
        return math.sqrt((Vec1[0] - Vec2[0]) ** 2 + (Vec1[1] - Vec2[1]) ** 2 + (Vec1[2] - Vec2[2]) ** 2)
    elif isinstance(Vec1, TQuaternion):
        # Return TFloat
        return math.sqrt((Vec1[0] - Vec2[0]) ** 2 + (Vec1[1] - Vec2[1]) ** 2 +
                         (Vec1[2] - Vec2[2]) ** 2 + (Vec1[3] - Vec2[3]) ** 2)
    else:
        Result = []
        minv = min(len(Vec1), len(Vec2))
        for f in range(minv):
            Result.append(Distance(Vec1[f], Vec2[f]))
        # Return TFloats
        return Result


# Vec1, Vec2 = TCoord
def DistanceSquared(Vec1, Vec2):
    # Return TFloat
    return (Vec1[0] - Vec2[0]) ** 2 + (Vec1[1] - Vec2[1]) ** 2 + (Vec1[2] - Vec2[2]) ** 2


# Coords = TCoords
def MidPoint(Coords):
    Result = basetypes.TCoord()
    for f in range(len(Coords)):
        Result = Add(Result, Coords[f])
    Result = Multiply(Result, 1 / len(Coords))
    # Return TCoord
    return Result


# First function: v1 = TCoord, v2 = TRotMatrix
# Second function: v1 = TCoords, v2 = TRotMatrix
# Third function: v1 = TCoord, v2 = TQuaternion
# Fourth function: v1 = TCoords, v2 = TQuaternion
def Rotate(v1, v2):
    if isinstance(v1, basetypes.TCoord):
        if isinstance(v2, TRotMatrix):
            # First function
            # Return TCoord
            return basetypes.TCoord(v2[0, 0] * v1[0] + v2[0, 1] * v1[1] + v2[0, 2] * v1[2],
                                    v2[1, 0] * v1[0] + v2[1, 1] * v1[1] + v2[1, 2] * v1[2],
                                    v2[2, 0] * v1[0] + v2[2, 1] * v1[1] + v2[2, 2] * v1[2])
        else:
            # Third function
            conjugate = Conjugated(v2)
            tmp = Quaternion(0, v1[0], v1[1], v1[2])
            tmp = Multiply(v2, tmp)
            tmp = Multiply(tmp, conjugate)
            # Return TCoord
            return basetypes.TCoord(tmp[1], tmp[2], tmp[3])
    else:
        Result = []
        if isinstance(v2, TRotMatrix):
            # Second function
            for f in range(len(v1)):
                Result.append(Rotate(v1[f], v2))
        else:
            # Fourth function
            conjugate = Conjugated(v2)
            for f in range(len(v1)):
                tmp = Quaternion(0, v1[f][0], v1[f][1], v1[f][2])
                tmp = Multiply(v2, tmp)
                tmp = Multiply(tmp, conjugate)
                Result.append(basetypes.TCoord(tmp[1], tmp[2], tmp[3]))
        # Return TCoords
        return Result


# Axis must be normalized, returns Quaternion defining rotation around axis
# Axis = TCoord, Rotation = TFloat
def RotationQuaternion(Axis, Rotation):
    cost = math.cos(Rotation / 2)
    sint = math.sin(Rotation / 2)
    # Return TQuaternion
    return Normalize(TQuaternion(cost, sint * Axis[0], sint * Axis(1), sint * Axis[2]))


# From http://www.gamedev.net/topic/429507-finding-the-quaternion-betwee-two-vectors/?p=3856228#entry3856228
# Vectors are assumed to be normalized
# VFrom, VTo = const TCoord
def RotationTo(VFrom, VTo):
    tmp = CrossProduct(VFrom, VTo)
    n1 = VFrom[0] ** 2 + VFrom[1] ** 2 + VFrom[2] ** 2
    n2 = VTo[0] ** 2 + VTo[1] ** 2 + VTo[2] ** 2
    dot = DotProduct(VFrom, VTo)
    # Return TQuaternion
    return Normalize(Quaternion(math.sqrt(n1 * n2) + dot, tmp[0], tmp[1], tmp[2]))


# Assumes same number of coords in both arrays
# Coords1, Coords2 = TCoords
def StaticRMSD(Coords1, Coords2):
    Result = 0
    for f in range(len(Coords1)):
        Result = Result + (Coords1[f][0] - Coords2[f][0]) ** 2 \
                        + (Coords1[f][1] - Coords2[f][1]) ** 2 \
                        + (Coords1[f][2] - Coords2[f][2]) ** 2
    # Return TFloat
    return math.sqrt(Result / len(Coords1))


# Two-dimensional distances

# From https://en.wikipedia.org/wiki/Line–line_intersection#Given_two_points_on_each_line
# x1, y1, x2, y2, x3, y3, x4, y4, Px, Py = TFloat,
def Intersection2D(x1, y1, x2, y2, x3, y3, x4, y4):
    num = (x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)
    den = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    Px = num / den
    num = (x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)
    Py = num / den
    # In Python return float and float
    return Px, Py


# From https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
# LineX1, LineY1, LineX2, LineY2, PointX, PointY, Normal = TFloat
def DistanceToLine2D(LineX1, LineY1, LineX2, LineY2, PointX, PointY, Normal=None):
    if Normal is None:
        Normal = math.sqrt((LineY2 - LineY1) ** 2 + (LineX2 - LineX1) ** 2)
    Result = (LineY2 - LineY1) * PointX - (LineX2 - LineX1) * PointY + LineX2 * LineY1 - LineY2 * LineX1
    if Result < 0:
        Result = - Result
    # Return TFloat
    return Result


# From http://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment
# Like distance to line, but checks extremities of segment
# x1, y1, x2, y2, Px, Py = TFloat
def DistanceToSegment2D(x1, y1, x2, y2, Px, Py):
    dsquare = (x1 - x2) ** 2 + (y1 - y2) ** 2
    if dsquare < basetypes.TypesConstants.Tiny: # Segment is a point
        # Return TFloat
        return math.sqrt((Px - x1) ** 2 + (Py - y1) ** 2)
    else:
        t = ((Px - x1) * (x2 - x1) + (Py - y1) * (y2 - y1)) / dsquare
        if t < 0: # Outside point 1
            # Return TFloat
            return math.sqrt((Px - x1) ** 2 + (Py - y1) ** 2)
        elif t > 1: # Outside point 2
            # Return TFloat
            return math.sqrt((Px - x2) ** 2 + (Py - y2) ** 2)
        else:
            projx = x1 + t * (x2 - x1)
            projy = y1 + t * (y2 - y1)
            # Return TFloat
            return math.sqrt((Px - projx) ** 2 + (Py - projy) ** 2)


# Distance to a normalized vector starting from the origin
# Axis, Point = const TCoord
def DistanceToNormalizedAxis(Axis, Point):
    vec = Subtract(Point, Axis)
    # Return TFloat
    return Norm(CrossProduct(Point, vec))


# Returns coordinate along axis and orthogonal to axis
# Axis, Point = const TCoord, x, y = TFloat
def OrthogonalCoords(Axis, Point):
    # In Python return float and float
    return DotProduct(Axis, Point), DistanceToNormalizedAxis(Axis, Point)


'''Old functions, with matrices'''


# Rotation = TRotMatrix, Place = TCoord, Vector = TCoord OR TCoords
def RotAndPlace(Rotation, Place, Vector):
    if isinstance(Vector, basetypes.TCoord):
        Result = Rotate(Vector, Rotation)
        # Return TCoord
        return Add(Result, Place)
    else:
        for f in range(len(Vector)):
            Vector[f] = RotAndPlace(Rotation, Place, Vector[f])


# R1 = TCoord, RotType = Integer
def BuildRotation(R1, RotType):
    if RotType is MCXYZRotation:
        # Return TRotMatrix
        return Multiply(Multiply(ZRotation(R1[2]), YRotation(R1[1])), XRotation(R1[0]))
    elif RotType is MCRotationXYAxis:
        Result = Multiply(YRotation(R1[1]), XRotation(R1[0]))
        # Return TRotMatrix
        return Multiply(InvertBase(Result), Multiply(ZRotation(R1[2]), Result))
    else:
        # Return TRotMatrix
        return IdentityMatrix


# Angle = TFloat
def XRotation(Angle):
    s = math.sin(Angle)
    c = math.cos(Angle)
    # Return TRotMatrix
    return TRotMatrix([1, 0, 0], [0, c, -s], [0, s, c])


# Angle = TFloat
def YRotation(Angle):
    s = math.sin(Angle)
    c = math.cos(Angle)
    # Return TRotMatrix
    return TRotMatrix([c, 0, s], [0, 1, 0], [-s, 0, c])


# Angle = TFloat
def ZRotation(Angle):
    s = math.sin(Angle)
    c = math.cos(Angle)
    # Return TRotMatrix
    return TRotMatrix([c, -s, 0], [s, c, 0], [0, 0, 1])


# M = TRotMatrix
def InvertBase(M):
    Result = TRotMatrix()
    for f in range(2):
        for g in range(2):
            Result[f, g] = M[g, f]
    # Return TRotMatrix
    return Result


# Vec1, Vec2 = TCoord
def BuildBase(Vec1, Vec2):
    Result = TRotMatrix()
    norm1 = Multiply(Vec1, 1 / Norm(Vec1))
    for f in range(2):
        Result[f, 0] = norm1[1]
    norm2 = Multiply(Vec2, 1 / Norm(Vec2))
    norm3 = CrossProduct(norm1, norm2)
    for f in range(2):
        Result[f, 2] = norm3[f]
    norm2 = CrossProduct(norm3, norm1)
    for f in range(2):
        Result[f, 1] = norm2[f]
    # Return TRotMatrix
    return Result
