import sys
import pathlib

tests_dir = pathlib.Path(__file__).parent
sys.path.insert(0, str(tests_dir.parent))

import overdata
