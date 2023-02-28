import matplotlib.pyplot as plt
import numpy as np

class HH:

    def __init__(self):
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

class Ellipse:

    def __init__(self): 

        fig, ax = plt.subplots(figsize=(12, 4))
        # ax.set_xlim(-1.5,1.5)
        ax.grid(True)


        x  = np.sin(np.linspace(-1, 1, num=30) * np.pi/2) 
        
        # ellipse  -----
        # x^2/a^2 + y^2/b^2 = 1
        # y = b * sqrt (1 - (x/a)^2)

        # norm --------------------
        a = 1
        b = 1
        # y = sqrt(1- x^2)
        y = 1 * np.sqrt (1 - x**2)
        ax.plot (x,y, label="norm" ) 

        # first oval ---------------------
        #  x^2/a^2 + y^2/(b^2 - o*x)= 1           # o = oval offset 
        #  b^2 = b^2  - o*x
        #  b = sqrt (b^2 - o*x) 
        # normed
        #  b = sqrt (1 - o*x)
        # y = sqrt (1 - o*x) * sqrt (1 - (x/a)^2)

        o = 0.3
        y = np.sqrt(1 - o *x) * np.sqrt (1 - x**2)
        ax.plot (x,y, label="norm o=%.1f" % (o )) 


        # stretch to tip -------------- 
        # stretch x to become y=tip at x= 1 
        # y = 1 * sqrt(1- 1/a^2)
        # a^2 = 1 / (1 - y^2)
        # a = sqrt (1 / (1 - y^2))
        # y = tip 
        # a = sqrt  (1 / (1 - tip ^2))

        tip = 0.4
        a = np.sqrt (1 / (1 - tip**2 / (1-o)))
        y =  np.sqrt(1 - o *x) * np.sqrt (1 - (x/a)**2)
        ax.plot (x,y, label="tip=%.2f o=%.2f a=%.3f " % (tip, o, a)) 


        # belly -------------------------

        yb = self._norm_tipBelly_function (x)
        ax.plot (x,yb, label="belly") 

        # yc = ((1.0-yb) * np.sqrt(1.0-x**2) + yb) 
        yc = ((1.0-yb) * y + yb) 
        ax.plot (x,yc, label="belly applied") 

        ax.legend()
        plt.show()

    def _norm_tipBelly_function (self,yArr):
        """
        the normalized chord distribution along the span
        :Args:
            :y: y-position 
        Returns:
            :chord: array of chord values 
        """
        self.ellipseBellyWidth = 0.5
        self.ellipseTipBelly = 0.2

        bellyArr = np.zeros(len(yArr))
        # calculate delta that will be added to pure ellipse
        for i, y in enumerate ( yArr):
            # calculate actual distance to tip
            distanceToTip = 1.0 - y
            if (distanceToTip > self.ellipseBellyWidth):
                # add constant value, as we are far away from the tip
                belly = self.ellipseTipBelly
            else:
                # add decreasing value according to quarter ellipse
                a  = self.ellipseBellyWidth
                y1 = self.ellipseBellyWidth - distanceToTip
                b  = self.ellipseTipBelly
                radicand = (a*a)-(y1*y1)
                if radicand > 0: belly = (b/a) * np.sqrt(radicand) # quarter ellipse formula
                else:            belly = 0
            bellyArr [i] = belly

        return bellyArr


# Main program for testing 
if __name__ == "__main__":

    Ellipse()