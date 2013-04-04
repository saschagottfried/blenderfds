#!/bin/bash
#
pylint --include-ids=y --disable=C0301,C0324,C0321,C0103 ../blenderfds > pylint_blenderfds_dev.txt

# Get help on error messages:
# pylint --help-msg R0913
