import basetypes
import geomhash
import geomutils
import math


# Returns Num points distributed around a sphere centered at origin, radius 1. Uses the Golden Section spiral algorithm,
# based on an implementation by Patrick Boucher at http://www.softimageblog.com/archives/115
# Num = Integer
def GoldenSpiralPoints(Num):
    Result = []
    inc = 3.6 / math.sqrt(Num)
    phi = 0
    Result.append(basetypes.TCoord(0, 0, -1))
    for k in range(1, Num - 1):
        z = -1 + 2 * k / (Num - 1)
        r = math.sqrt(1 - z * z)
        phi = phi + inc / r
        Result.append(basetypes.TCoord(math.cos(phi) * r, math.sin(phi) * r, z))
    Result.append(basetypes.TCoord(0, 0, 1))
    # Return TCoords
    return Result


# Based on the Shrake-Rupley ASA algorithm. SpherePoints is the base set of points at the surface of a radius 1 sphere,
# centered at the origin, to estimate surface area. Returns the surface area for each sphere defined by Points and
# Radii. MinHashCell is the minimum size of the geometric hash grid cells, which is also at least as large as twice the
# largest atomic radius.
# NOTE: for ASA add probe radius to radius of each atom
# Points = TCoords, Radii = TFloats, SpherePoints = TCoords, MinHashCell = Single
def SRSurface(Points, Radii, SpherePoints, MinHashCell = 0):
    Result = []
    if Points is None:
        # Return TFloats
        return Result
    else:
        hashcell = LargestRadius(Radii) * 2
        if hashcell < MinHashCell:
            hashcell = MinHashCell
        ghash = geomhash.TGeomHasher(Points, hashcell)
        for f in range(len(Points)):
            ixs = ghash.ListNeighbours(Points[f])
            countouts = 0
            for g in range(len(SpherePoints)):
                sphere = geomutils.Add(geomutils.Scaled(SpherePoints[g], Radii[f]), Points[f])
                intersect = False
                for h in range(len(ixs)):
                    if (ixs[h] is not f) and (geomutils.Distance(sphere, Points[ixs[h]]) < Radii[ixs[h]]):
                        intersect = True
                        break
                if not intersect:
                    countouts = countouts + 1
            Result.append(4 * geomutils.PI * (Radii[f]) ** 2 / len(SpherePoints) * countouts)
        # Return TFloats
        return Result


# Radii = TFloats
def LargestRadius(Radii):
    Result = 0
    for f in range(len(Radii)):
        if Radii[f] > Result:
            Result = Radii[f]
    # Return TFloat
    return Result
