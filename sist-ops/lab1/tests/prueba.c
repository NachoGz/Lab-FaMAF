#include <stdio.h>
#include "../execute.h"

int main(void) {
    const char* test_path="/foo/bar";
    scommand cd_cmd = scommand_new ();
    scommand_push_back (cd_cmd, strdup ("cd"));
    scommand_push_back (cd_cmd, strdup (test_path));
}
