"""

Start app with 'python -m <package>' when installed as package  

"""

import sys
from . import app

if __name__ == '__main__':
    sys.exit(app.start())  