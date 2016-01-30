if type pip &>/dev/null
then
    echo pip installed, ... exiting
    exit
fi


# get pip
wget https://bitbucket.org/pypa/setuptools/raw/bootstrap/ez_setup.py
python ez_setup.py –user
