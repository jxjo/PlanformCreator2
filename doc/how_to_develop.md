## Tips to Setup Development

If you would like to contribute to this project and are not too familiar with Python development for GitHub repositories, you'll find some basic tips here for an easy start.

Before we start development, a few installations have to be done: 
- Install the Git-Client to connect to GitHub
- Install VS Code (recommended) as the integrated software development environment
- Clone the project from GitHub 

Then we setup VS Code, to run the app directly within VS Code
- Config to launch the app
- Debugging 

---
### Git Client for VS Code

#### Windows

1. Install Git: Download and install Git from [git-scm.com](https://git-scm.com/)
2. Configure Git: Open Command Prompt or PowerShell and set your user information:
    ```cmd
    git config --global user.name "Your Name"
    git config --global user.email "your.email@example.com"
    ```
3. **VS Code Integration**: VS Code automatically detects Git if it's installed in your system PATH
4. **Verify Setup**: Open the Source Control panel in VS Code (Ctrl+Shift+G) to confirm Git is recognized
5. **Clone Repository**: Use `Git: Clone` from the Command Palette (Ctrl+Shift+P) or clone via Command Prompt:
    ```cmd
    git clone https://github.com/username/AirfoilEditor.git
    ```

#### Linux

1. Install Git: Install Git using your distribution's package manager:
    ```bash
    # Debian/Ubuntu
    sudo apt-get install git
    
    # Fedora
    sudo dnf install git
    
    # Arch Linux
    sudo pacman -S git
    ```
2. Configure Git: Open a terminal and set your user information:
    ```bash
    git config --global user.name "Your Name"
    git config --global user.email "your.email@example.com"
    ```
3. **VS Code Integration**: VS Code automatically detects Git if it's installed
4. **Verify Setup**: Open the Source Control panel in VS Code (Ctrl+Shift+G) to confirm Git is recognized
5. **Clone Repository**: Use `Git: Clone` from the Command Palette (Ctrl+Shift+P) or clone via terminal:
    ```bash
    git clone https://github.com/username/AirfoilEditor.git
    ```

### Installing VS Code and Extensions

#### Windows

- Download the installer from [code.visualstudio.com](https://code.visualstudio.com/)
- Run the installer and follow the setup wizard
- Check "Add to PATH" during installation for command-line access

#### Linux

- Download the `.deb` or `.rpm` package from [code.visualstudio.com](https://code.visualstudio.com/)
- Or install via package manager:
    ```bash
    # Debian/Ubuntu
    sudo snap install code --classic
    
    # Fedora
    sudo dnf install code
    ```

#### Essential Extensions for Python Development

Open VS Code and install these extensions via the Extensions panel (Ctrl+Shift+X):

1. **Python** (by Microsoft) - Core Python language support, IntelliSense, debugging, and linting
2. **Pylance** (by Microsoft) - Fast Python language server for enhanced type checking and auto-completion
3. **Python Debugger** (by Microsoft) - Debugging support for Python applications
4. **GitLens** (by GitKraken) - Enhanced Git capabilities, inline blame annotations, and repository insights
5. **autoDocstring** (by Nils Werner) - Quickly generate Python docstrings

To install, search for each extension name in the Extensions panel and click "Install".

---


### Clone from GitHub

Now you are ready to clone the project from Github.
VS Code automatically detects Git if it's installed.

First let's verify, if VS Code is recognized: Open the Source Control panel in VS Code (Ctrl+Shift+G) to confirm Git is recognized. Then we clone (copy) the repository from GitHub:

**Clone Repository**: 

Use `Git: Clone` from the Command Palette (Ctrl+Shift+P) 

or clone via terminal: 
    ```
    git clone https://github.com/username/AirfoilEditor.git
    ```

Sometimes VS Code claims, that this directory is unsafe and cannot be accessed. In the case add the directory to the trusted repositories with the command (the directory is just an exmaple) 

```
git config --global --add safe.directory 'e:/github/airfoileditor'
```

---

Now we are ready for development.

