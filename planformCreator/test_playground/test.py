import numpy as np
import matplotlib.pyplot as plt

# Main program for testing -----------------------------------

if __name__ == "__main__":

    fig, ax = plt.subplots(figsize=(10, 6))

    y = np.random.randint(0, 100, size=50)
    x = np.random.choice(np.arange(len(y)), size=10)

    line, = ax.plot(y, '-', label='my super line')
    dot, = ax.plot(x, y[x], 'o', label='dot')

    legend = plt.legend(loc='upper right')
    line_legend, dot_legend = legend.get_lines()
    line_legend.set_picker(True)
    line_legend.set_pickradius(10)
    dot_legend.set_picker(True)
    dot_legend.set_pickradius(10)

    line.set_picker (True)
    line.set_pickradius(10)


    graphs = {}
    graphs[line_legend] = line
    graphs[dot_legend] = dot


    def on_pick(event):
        artist = event.artist
        if artist == line:
            print ("line clicked ", artist.get_label())
        else:
            print("legend: ",event)

            legend = artist
            isVisible = legend.get_visible()

            graphs[legend].set_visible(not isVisible)
            legend.set_visible(not isVisible)

            fig.canvas.draw()

    plt.connect('pick_event', on_pick)
    plt.show()
