#!/usr/bin/env python
# -*- coding: utf-8 -*-

#  Copyright (C) 2020-2022 Matthias Boese

"""  

Utility functions for dxf import

"""

import ezdxf
from ezdxf.addons import Importer
import numpy as np
from math import atan2,degrees
from common_utils import *

# setup matching range to 1% of length of rootchord
matching_range = 0.1    # the original value caught the wrong line 200.0 / 100.0

#-------------------------------------------------------------------------------
# all the specials for DXF import  
#-------------------------------------------------------------------------------


def distance_between(p1, p2):
    '''calculates the distance between two given points'''
    x1, y1 = p1
    x2, y2 = p2
    dist = np.sqrt(np.square(x2-x1) + np.square(y2-y1))
    return dist


def line_angle(p1, p2):
    '''calculates angle clockwise between two given points'''
    x1, y1 = p1
    x2, y2 = p2
    GK = (y2-y1)
    AK = (x2-x1)
    return degrees(atan2(GK, AK))
    

def convert(xy, x_offset, y_offset, scaleFactor_x, scaleFactor_y):
    x, y = xy
    x -= x_offset
    x *= scaleFactor_x
    y -= y_offset
    y *= scaleFactor_y

    return (x,y)

def get_rootline(lines):
    lengths = []
    idxList = []
    rootline = None

    num = len(lines)
    TraceMsg("trying to find rootline, number of lines: %d" % num)

    # check all lines
    for idx in range(num):
        line = lines[idx]

        # check angle, get first and last point of line (no matter, if polyline)
        p1, p2 = (line[0]), (line[-1])
        length = abs(distance_between(p1, p2))
        angle_abs = abs(line_angle(p1, p2))
        TraceMsg("checking line %d, length: %f, angle: %f" % (idx, length, angle_abs))
        
        if ((angle_abs>89.9) and (angle_abs<90.1)):
            TraceMsg("appending line %d to candidate list" % idx)
            # line runs nearly straight up or straight down, so is a candidate.
            # calculate length
            length = abs(distance_between(p1, p2))
            lengths.append(length)
            idxList.append(idx)
    
    # are there any candidates in the list ?
    if (len(idxList) != 0):
        # yes, find candidate with maximum length
        maxLength = max(lengths)
        idx = lengths.index(maxLength)
        rootlineIdx = idxList[idx]
        line = lines[rootlineIdx]
        
        # copy only first and last point to rootline, in case we have a polyline
        rootline = [line[0], line[-1]]
        
        # remove from list of lines
        lines.pop(rootlineIdx)
        lengths.pop(idx)

        TraceMsg("found rootline, length is %f, idx is %d" % (maxLength, rootlineIdx))
    
        # check if rootline has to be reverted
        (x1, y1), (x2, y2) = rootline
        if (y2 < y1):
            # revert line
            TraceMsg("reverting rootline")
            rootline = rootline[::-1]

    return rootline, lines

def get_hingeline(rootline, lines):
        
    hingeline = None

    num = len(lines)
    TraceMsg("trying to find hingeline, number of lines: %d" % num)

    # get x-coordinates of rootline
    (root_x1, root_y1), (root_x2, root_y2) = rootline

    # check all lines
    for idx in range(num):
        line = lines[idx]
        
        # get coordinates of start- and endpoint
        line_x1, line_y1 = line[0]
        line_x2, line_y2 = line[-1]
        
        # check x-ccordinates for match with rootline
        if ((abs(root_x1-line_x1) <= matching_range) or
            (abs(root_x1-line_x2) <= matching_range)):
            x_match = True
        else:
            x_match = False

        # check y-ccordinates for match with rootline
        if ((abs(root_y1-line_y1) <= matching_range) or
            (abs(root_y1-line_y2) <= matching_range) or
            (abs(root_y2-line_y1) <= matching_range) or
            (abs(root_y2-line_y2) <= matching_range)):
            y_match = True
        else:
            y_match = False
        
        # hingeline must have same x-coordinate, but must not have same y-coordinate as root
        if ((x_match == True) and (y_match == False)):
            TraceMsg("Found hingeline, idx: %d" % idx)
            # found hingeline
            hingeline = lines[idx]
        
            # remove from list of lines
            lines.pop(idx)
        
            # check if hingeline has to be reverted
            if (line_x2 <line_x1):
                # revert line
                TraceMsg("reverting hingeline")
                hingeline = hingeline[::-1]
            return hingeline, lines
    
    TraceMsg("no hingeline was found")
    return hingeline, lines

def __points_match(p1, p2):
    global matching_range
    x1, y1 = p1
    x2, y2 = p2

    if ((abs(x2-x1) <= matching_range) and
        (abs(y2-y1) <= matching_range)):
        return True
    else:
        return False

def __get_matching_line(point, lines):
    x,y = point
    TraceMsg("searching for line with start- or endpoint %f, %f" % (x, y))
    
    # check number of lines
    num = len(lines)
    if num == 0:
        return None, None
    
    # search in all lines
    for idx in range(num):
        line = lines[idx]
        
        p1 = line[0]
        p2 = line[-1]
        x1, y1 = p1
        x2, y2 = p2

        TraceMsg("checking line %d. Startpoint: %f, %f, Endpoint %f, %f" % (idx, x1, y1, x2, y2))
        if __points_match(point, p1):
            # return idx and line as is
            TraceMsg("found matching startpoint")
            return idx, line
        elif __points_match(point, p2):
            # line has to be reverted
            TraceMsg("found matching endpoint, line has to be reverted")
            line = line[::-1]
            return idx, line
        
    # nothing was found
    ErrorMsg("No matching line was found")
    return None, None

    
def create_contour(rootline, lines):
    # join all lines to contour
    contour = []
    actual_point = rootline[0]
    endpoint = rootline[1]
    
    while True:
        # check lines
        idx, line  = __get_matching_line(actual_point, lines)
        
        if idx != None:
            # remove from list of polylines
            lines.pop(idx)
        
        # found a matching line ?
        if line == None:
            ErrorMsg("no matching line was found, contour could not be finished")
            return contour, lines
        else:    
            # append to contour - take only additional point of line to contour
            #                     to avoid dublicate points 
            if not contour:
                contour = list(line)                    # first line (tuple) complete
            else:
                for point in line:
                    if point != contour[-1]:            # avoid dublicate points 
                        contour.append(point) 
                # if line [0] != contour [-1]:            # avoid dublicates
                #     contour.append(line [0])
                # contour.append(line[1])                 # 2nd point of line always
            
            # set new actual point, which is the endpoint of the current line
            actual_point = line[-1]
            x, y = actual_point
            
            # check if we have reached the endpoint
            if __points_match(endpoint, actual_point):
                x,y = endpoint
                # we have finished
                return contour, lines


def split_contour(contour):
    # split contour into leading edge and trailing edge
    LE = []
    TE = []
    num = len(contour)
    
    if (num == 0):
        ErrorMsg("__split_contour: contour has no points")
        return (LE, TE)
    
    # initialize max_x with root at TE
    max_x, y = contour[0]
    
    # first point always TE
    TE.append(contour[0])
    
    for idx in range(1, num):
        x, y = contour[idx]
        # determine idx of max x value
        if (x >= max_x):
            # if we have not reached maximum, append to LE
            max_x = x
            maxIdx = idx
            TE.append(contour[idx])
        else:
            # after we have reached maximum append to TE
            LE.append(contour[idx])
    
    # revert LE
    LE = LE[::-1]

    # join the two lines in the middle
    (x_LE_last, y_LE_last) = LE[-1]
    (x_TE_last, y_TE_last) = TE[-1]
    
    y_join = (y_LE_last + y_TE_last)/2
    x_join = max(x_LE_last, x_TE_last)
    p_join = (x_join, y_join)

    LE[-1] = p_join
    TE[-1] = p_join

    return LE, TE  

def __line_isDuplicate(line1, line2):
    # check endpoints
    if ((line1[0] == line2[0]) and
    (line1[-1] == line2[-1])):
        return True
    else:
        return False

def __find_duplicates(line_idx, lines):
    duplicates = []
    for idx in range(line_idx+1, len(lines)):
        if __line_isDuplicate(lines[line_idx], lines[idx]):
            TraceMsg("found duplicate line, idx %d" % idx)
            duplicates.append(idx)

    return duplicates

def remove_duplicate_lines(lines):
    duplicates = []

    # find all duplictes, append idx to list
    for idx in range(len(lines)):
        duplicates.extend(__find_duplicates(idx, lines))
    
    # remove all duplicates
    duplicates = duplicates[::-1]
    for idx in duplicates:
        TraceMsg("removed duplicate line, idx %d" % idx)
        lines.pop(idx)
    
    return lines


def __convert_toPlanform(msp):
    num_segments = 500

    # empty list of lines
    myLines = []

    '''TraceMsg("Analysing entities in dxf file")
    for e in msp:
        TraceMsg("found entity %s" % e.dxftype())'''
    
    # get all inserts and explode
    inserts = msp.query("INSERT")
    for insert in inserts:
        insert.explode()

    # get all polylines and explode
    polylines = msp.query("POLYLINE")
    for line in polylines:
        points = line.points()
        newLine = []
        for point in points:
            x,y,z = point
            p = (x,y)
            # append point to line
            newLine.append(p)
        # append line to list of lines
        myLines.append(newLine)   
        #line.to_spline()#explode()

    # get all arcs and convert to spline
    arcs = msp.query("ARC")
    for arc in arcs:
        arc.to_spline()

    for e in msp:
        TraceMsg("found entity %s" % e.dxftype())
        
    # get all lines
    lines = msp.query("LINE")
    
    # evaluate all lines and append to myLines
    idx = 0
    for line in lines:
        TraceMsg("getting line %d:" % idx)
        x1, y1, z = line.dxf.start
        x2, y2, z = line.dxf.end
        myLines.append(((x1, y1), (x2, y2)))   
        idx += 1
    
    # get all splines
    splines = msp.query("SPLINE")

    # create new modelspace
    doc = ezdxf.new('R2010')
    msp_new = doc.modelspace()

    # evaluate all splines and convert into polylines
    idx = 0
    for spline in splines:
        TraceMsg("getting spline %d and converting to 2d polyline with %d segments" % (idx, num_segments))
        bspline = spline.construction_tool()
        xy_pts = [p.xy for p in bspline.approximate(segments=num_segments)]
        msp_new.add_lwpolyline(xy_pts, format='xy')
        idx += 1

    # get all lw polylines 
    lw_polylines = msp.query("LWPOLYLINE")
    lw_polylines.extend(msp_new.query("LWPOLYLINE"))
    
    # evaluate all lw polylines and append to myLines
    idx = 0
    for line in lw_polylines:
        TraceMsg("getting lw polyline %d" % idx)
        with line.points("xy") as points:
            # append points of polyline
            myLines.append(points)    
            idx += 1
    
    return myLines

def _normalize_lines (lines, y_offset, scaleFactor_y):  
    """ noramlize lines (LE oder TE) to have x = 0..1 and y=0..1"""

    x1, y1 = lines[0]                       # root point
    x2, y2 = lines[-1]                      # tip point

    scaleFactor_x = 1.0 / abs(x2-x1)
    x_offset = x1

    lines_norm = []       
 
    for point in lines :  
        point_norm = convert (point, x_offset, y_offset, scaleFactor_x, scaleFactor_y)
        lines_norm.append(point_norm)

    # ensure x = 1 for last point 
    tip = lines_norm [-1]
    lines_norm [-1] = ((1.0, tip[1]))

    return lines_norm


def __create_planformShape(lines):
    
    TraceMsg("creating planformshape")

    # remove duplicates, if any
    remaining_lines = remove_duplicate_lines(lines)  

    # get rootline
    rootline, remaining_lines = get_rootline(lines)
    
    if rootline == None:
        ErrorMsg("root line not found")
        return None
        
    # calculate rootchord, determine scale factor and offsets    
    (x1, y1) , (x2, y2) = rootline
    rootchord = y2 - y1
    scaleFactor_y = 1 / rootchord
    y_offset = y1

    # get hingeline, if any
    hingeline, remaining_lines = get_hingeline(rootline, remaining_lines)

    # join remaining lines and polylines to contour
    contour, remaining_lines = create_contour(rootline, remaining_lines)
    
    # split contour into leading edge and trailing edge
    LE, TE = split_contour(contour)       

    # check number of points
    if (len(LE) == 0):
        ErrorMsg("number of LE points is zero")
        return None

    if (len(TE) == 0):
        ErrorMsg("number of TE points is zero")
        return None
    
    LE_norm = _normalize_lines (LE, y_offset, scaleFactor_y) 
    TE_norm = _normalize_lines (TE, y_offset, scaleFactor_y)  
    HL_norm = _normalize_lines (hingeline, y_offset, scaleFactor_y )
                
    # calculate angle of hingeline
    if (hingeline != None):
        p1 = hingeline[0]
        p2 = hingeline[-1]
        hingelineAngle = line_angle(p1, p2)
        TraceMsg(" hingeline angle: %f°" %(hingelineAngle))
    else: 
        hingelineAngle = None
    
    return LE_norm, TE_norm, HL_norm, hingelineAngle



def import_fromDXF(FileName):
    try:
        sdoc = ezdxf.readfile(FileName)
    except:
        return None, None, None, None

    tdoc = ezdxf.new()
    importer = Importer(sdoc, tdoc)

    # import all entities from source modelspace into modelspace of the target drawing
    importer.import_modelspace()

    # import all paperspace layouts from source drawing
    importer.import_paperspace_layouts()

    # import all CIRCLE and LINE entities from source modelspace into an arbitrary target layout.
    # create target layout
    tblock = tdoc.blocks.new('SOURCE_ENTS')

    # query source entities
    ents = sdoc.modelspace().query('CIRCLE LINE')

    # import source entities into target block
    importer.import_entities(ents, tblock)

    # This is ALWAYS the last & required step, without finalizing the target drawing is maybe invalid!
    # This step imports all additional required table entries and block definitions.
    importer.finalize()
    
    # Convert data to planform and chord distribution
    lines = __convert_toPlanform(sdoc.modelspace())

    # Extract leading- and trailing edge, hinge line - uff!
    result = __create_planformShape(lines)
    if not result is None: 
        LE_norm, TE_norm, HL_norm, hingelineAngle = (result)
        return LE_norm, TE_norm, HL_norm, hingelineAngle
    else: 
        return None, None, None, None
    

