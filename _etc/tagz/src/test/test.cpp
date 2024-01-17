#include <iostream>
#include <string>
#include "tagz/html/tag.hpp"
#include "tagz/xterm/tag.hpp"

using namespace std;
using namespace tagz;
using namespace tagz::xterm;

int main()
{
    string test =
        html::div {
            "some text",
            html::a { "link" },
            html::div {
                "more text"
            }
        };
    cout << test << endl;
    cout << (string)red { "Hello, " } << (string)ul { "world" } << endl;
    return 0;
}

