#-------------------------------------------------------------------------------




class Planform_Elliptical_HH (Planform):
    """ 
    Defines the outline of an elliptical planform adapted with Hicks-Henne functions 
    """
    planformType  = "elliptical HH"
    isTemplate    = True                        # user may make mods 

    shortDescription = "Elliptical based planform modified by Hicks Henne functions.\n" + \
                       "Therefore either position or chord of a section can be defined."

    # is the planform defined by wing section or vice versa - overwrite 
    sections_depend_on_planform = True           # e.g.elliptical    
    planform_depend_on_sections = False          # e.g trapezoid

    wingSection_eitherPosOrChord = True

    def _norm_chord_corr_function (self,y):
        """
        The delta, a user can add to the norm_chord.  
        Currently by a hicks Henne shape function 
        :Args:
            :y: y-position 
        Returns:
            :dx: HH delta value 
        """
        alpha = self.HH_height  / 10                    # reduce to 0..0.1  - for user 0..1 
        t2    = (2 * (1-self.HH_width)) ** 2 + 1        # width  5..1 - for user 0..1
        t1    = self.HH_position                        # location 0..1

        # HH function - (coordinate system is inverse at the wing) 
        dx = alpha * (np.sin (np.pi * y ** (np.log(0.5)/np.log(t1)))) ** t2

        return dx


    def __init__(self, myWing: Wing, dataDict: dict = None):
        super().__init__(myWing, dataDict)
        """
        Args:
            :myWing: the wing object self belongs to  
            :dataDict: optional - dictonary with all the data for a valid section
        """
        # read additional parameters of this shapeform 
        self._leCorrection  = fromDict (dataDict, "leCorrection", 0, False)
        self._ellipseShift           = fromDict (dataDict, "ellipseShift", 0, False)


    # ---Properties --------------------- 

    @property
    def leCorrection(self): return self._leCorrection
    def set_leCorrection(self, newVal): self._leCorrection = newVal

    @property
    def ellipseShift(self): return self._ellipseShift
    def set_ellipseShift(self, newVal): self._ellipseShift = newVal

    # chord correction with Hicks Henne HH
    @property
    def HH_height(self): return self._HH_height
    def set_HH_height(self, newVal): 
        """ Hicks Henne correction - height 0..1  """
        self._HH_height = newVal

    @property
    def HH_position(self): return self._HH_position
    def set_HH_position(self, newVal): 
        """ Hicks Henne correction - position 0..1  """
        newVal = max (0.01, newVal)
        newVal = min (0.99, newVal)
        self._HH_position = newVal

    @property
    def HH_width(self): return self._HH_width
    def set_HH_width(self, newVal): 
        """ Hicks Henne correction - width 0..1  """
        self._HH_width = newVal

    # ---functions --------------------- 
    def norm_chord_function (self, y_norm):
        """
        Returns the normalized chord of the planform at y 0..1
        Args:
            :y_norm: the y-Position in the planform 0..1
        Returns:
            :chord: the chord 0..1 at y
        """

        # Stretch - coorection factor of yPos - the ellipse will be stretched to have 
        #           a normalized tipchord 
        tipChord_norm = self.wing.tipchord / self.wing.rootchord
        tipChord_norm = min (tipChord_norm, 0.5)        # limit to 50% root 
        yStretched = y_norm * np.sqrt(1.0-(tipChord_norm**2))

        # Shift -   elliptical shaping of the wing plus additonal delta
        #           x-value is shifted so it's not a full quarter ellipe to have an 
        #           LE angle > 0 at root 
        yShifted  = interpolate(0, 1.0, self.ellipseShift, 1, yStretched)


        chord =  (np.sqrt(1.0-( yShifted     * yShifted    )) ) / (
                  np.sqrt(1.0-(self.ellipseShift * self.ellipseShift)))

        # add Hicks Henne shape function 
        chord = chord + self._norm_chord_corr_function (y_norm)

        chord = min(1.0, chord)         # chord shouldn't be more than chord          

        return chord


    def _planform_function (self, y):
        """
        calculates all relevant geo data of the planform at y 

        Args:
            :y: the y-Position in the planform 0.. halfSpanWidth
        Returns:
            :leadingEdge:  ...-point at y
            :trailingEdge: ...-point at y
        """
        hingeRootx = (1-(self.flapDepthRoot/100)) * self.rootchord
        hingeTipx = hingeRootx +  \
                            np.tan((self.hingeAngle/180) * np.pi) * self.halfwingspan

        # chord-length at y
        y_norm = y / self.halfwingspan
        chord =  self.norm_chord_function (y_norm) * self.rootchord 

        # calculate hingeDepth in percent at this particular point along the wing
        hingeDepth_y = interpolate(0.0, self.halfwingspan,
                                    self.flapDepthRoot, self.flapDepthTip,y)

        # correction of leading edge for elliptical planform, avoid swept forward part of the wing
        delta = (self.leCorrection) \
                 * sin(interpolate(0.0, self.halfwingspan, 0.0, np.pi, y)) \
                 * 0.1 * self.rootchord        # scale to proper result 0..1 

        flapDepth = (hingeDepth_y/100) * chord + delta 

        # finally the main "lines" at position y
        hinge        = (hingeTipx-hingeRootx)/(self.halfwingspan) * (y) + hingeRootx
        leadingEdge  = hinge - (chord - flapDepth)
        trailingEdge = leadingEdge + chord

        return leadingEdge, trailingEdge


#-------------------------------------------



class Edit_Planform_Elliptical_HH (Edit_Abstract):
    """ 
    Frame to edit the parameters of a elliptical planform
    """
    name = Planform_Elliptical_HH.planformType

    def init(self, dataObject = None):

        self.planform : Planform_Elliptical_HH = dataObject

        self.grid_columnconfigure   (0, weight=0)
        self.grid_rowconfigure      (6, weight=1)

        self.add (Field_Widget  (self,0,0,    lab="HH height",  get=lambda: self.planform.HH_height, 
                                    set=self.planform.set_HH_height,
                                    lim=(-0.5,1), dec=2, spin=True, step=0.05,
                                    event=CHORD_CHANGED))
        self.add (Field_Widget  (self,0,3,    lab="position", lab_width=60,
                                    get=lambda: self.planform.HH_position, set=self.planform.set_HH_position,
                                    lim=(0.2,0.95), dec=2, spin=True, step=0.05,
                                    event=CHORD_CHANGED))
        self.add (Field_Widget  (self,1,3,    lab="width", lab_width=60,
                                    get=lambda: self.planform.HH_width, set=self.planform.set_HH_width,
                                    lim=(0.1,1.0), dec=2, spin=True, step=0.05,
                                    event=CHORD_CHANGED))

        self.add (Field_Widget  (self,2,0,    lab="Ellipse shift",   
                                    get=lambda: self.planform.ellipseShift, set=self.planform.set_ellipseShift,                                    
                                    lim=(0,1), dec=2, spin=True, step=0.05,
                                    event=CHORD_CHANGED))

        self.add (Field_Widget  (self,3,0,    lab="LE correction",   
                                    get=lambda: self.planform.leCorrection, set=self.planform.set_leCorrection,
                                    lim=(-1,1), dec=2, spin=True, step=0.05,
                                    event=PLANFORM_CHANGED))



# Main program for testing 
if __name__ == "__main__":

    import matplotlib.pyplot as plt
    import numpy as np

    def t2f (t2norm):
        return (2 * (1-t2norm)) ** 2 + 1

    fig, ax = plt.subplots(figsize=(10, 6))
    ax.set_ylim(0,0.2)
    ax.grid(True)

    alpha = 0.1                     # height
    t1    = 0.8                    # location 
    t2    = 2                     # width
    x = np.linspace (0.0, 1.0, 500) 

    t2norm = 0.0
    t2 = t2f (t2norm)
    y = alpha * (np.sin (np.pi * x ** (np.log(0.5)/np.log(t1)))) ** t2
    ax.plot (x,y, label="t2norm=%.2f  %.2f" % (t2norm, t2)) 

    t2norm = 0.4
    t2 = t2f (t2norm)
    y = alpha * (np.sin (np.pi * x ** (np.log(0.5)/np.log(t1)))) ** t2
    ax.plot (x,y, label="t2norm=%.2f  %.2f" % (t2norm, t2)) 

    t2norm = 0.8
    t2 = t2f (t2norm)
    y = alpha * (np.sin (np.pi * x ** (np.log(0.5)/np.log(t1)))) ** t2
    ax.plot (x,y, label="t2norm=%.2f  %.2f" % (t2norm, t2)) 

    t2norm = 1
    t2 = t2f (t2norm)
    y = alpha * (np.sin (np.pi * x ** (np.log(0.5)/np.log(t1)))) ** t2
    ax.plot (x,y, label="t2norm=%.2f  %.2f" % (t2norm, t2)) 




    ax.legend()
    plt.show()
