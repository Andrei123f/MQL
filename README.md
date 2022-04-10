<div id="top"></div>


[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]



<!-- PROJECT LOGO -->
<br />
<div align="center">
  <h3 align="center">MQL - SQL language for CSV files.</h3>

  <p align="center">
    Imperative SQL language for CSV files made with a functional programming language, <img src="https://assets.website-files.com/62126aa407695905a4f073d0/621bd9bc08086c73487dce88_haskell-original.svg" alt="Vue logo" width=20 height=20>Haskell.
    <br />
    <a href="https://replit.com/@Andrei123f/MQL#README.md/">View Demo</a> <br />
    <a href="https://github.com/Andrei123f/MQL/issues">Report Bug</a><br />
    <a href="https://github.com/Andrei123f/MQL/issues">Request Feature</a> <br />
    **For the demo please follow the instructions from the README from there.**
  </p>
</div>



<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#how-does-it-work">How Does RenderLingo work</a></li>
        <li><a href="#built-with">Built With</a></li>
        <li><a href="#full-features">Features of MQL</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project
MQL is an imperative SQL language interpretor built using the functional programming language, <img src="https://assets.website-files.com/62126aa407695905a4f073d0/621bd9bc08086c73487dce88_haskell-original.svg" alt="Vue logo" width=20 height=20> Haskell. Some of the features of MQL include type checking, type switching, for full features please see the <a href="#full-features">features section</a>.


### How Does MQL work
<div id="how-does-it-work"></div>
⚙️ MQL uses Alex as lexical analyser generator to create the tokens and Happy as parser generator to create the parse tree. After that, the parse tree will be passed 
to Evaluator.hs where the semantics of the program will be decided.
<br />
<br />
The flow would look like this: <br />
Source code => Alex(outputs tokens) => Happy(outputs parse tree) => Evaluator(outputs the result)



### Built With
The major tools that I used: <br />
* <a href="https://www.haskell.org/happy/"><img src="https://www.haskell.org/happy/Happy.gif" alt="Happy logo" width=20 height=20>Happy</a> 
* <a href="https://www.haskell.org/alex/">Alex</a> 
<p align="right">(<a href="#top">back to top</a>)</p>

### Featues of MQL
<div id="full-features"></div>
Main features of MQL: <br />
Types <br />
<ul>
  <li>int</li>
  <li>float</li>
  <li>boolean</li>
  <li>row (behind the scenes just a list of strings)</li>
  <li>table (behind the scenes just a list of rows, so, list of list of strings)</li>  
</ul>
<br />
<br />

Scoping
MQL has dynamic scoping, as the variables must be all declared globally at the beginning of the program before the logical part. <br />
When declaring a variable you do not need necessarily to declare them on separate lines, but at the end the declaration there must be a semicolon as such:
  ```sh
  boolean flag1 = true; boolean flag2 = false;
  ```
  
Helper functions && Type switching/checking
MQL includes core helper functions to help the user to manipulate the data from the CSV files:
<ul>
  <li>row.putElement(el): puts the element "el" at the end of the row "row"</li>
  <li>row.getElement(index): gets the element that is situated at the respective index in the row</li>
  <li>row1.mergeRow(row2): merges row1 and row2</li>
  <li>row.resetRow(): resets the row</li>
  <li>table.getRow(index): gets the row that is situated at the respective index in the table</li>
  <li>table.putRow(row): puts the row "row" at the end of the table "table"</li>
  <li>table.asc(): sorts lexico graphically a table</li>
  <li>table.desc(): sorts desc lexico graphically a table</li>
</ul>

MQL also includes type checking functions:
* el.isInt() - this will return true if el is an int and false otherwise
``` sh
string foo = "5";
string bar = "notAnInt";
boolean result1 = foo.isInt(); -- this will return true
boolean result2 = bar.isInt(); -- this will return false
```
* el.isString() - this will return true if el is a string and false otherwise
* el.isFloat() - this will return true if el is a float and false otherwise

MQL also includes type switching functions:
* el.toString() - this will return the string value with the string type of the element "el" 
* el.getInt() - this will return the int value with the int type of the element "el" 
* el.getFloat() - this will return the float value with the float type of the element "el" 
``` sh
int foo1 = 10; float foo2 = 10.50; string foo3 = "10"; string foo4 = "10.50";
string bar1;   string bar2;        int bar3;           float bar4;
bar1 = foo1.toString(); 
bar2 = foo2.toString();
bar3 = foo3.getInt();
bar4 = foo4.getFloat();
```
In the above example, bar1 is a string with the value "10", bar2 is a string with the value "10.50", bar3 is an int with the value 10 and bar4 is a float with the value 10.50.

Below is a snapshot of how the type switching/checking function could be used in a flow:
``` sh
... declared table A, row aRow, string aEl, int aInt
aRow = A.getRow(0);
aEl = aRow.getElement(0);

if(aEl.isInt())
    aInt = aEl.getInt();
    ...do some stuff with aInt (you can now do addition, substraction, etc...)
endif;

... rest of logic
```
<p align="right">(<a href="#top">back to top</a>)</p>



<!-- GETTING STARTED -->
## Getting Started

You will need <a href="https://www.haskell.org/downloads/">haskell</a> and <a href="https://hackage.haskell.org/package/cabal-install">cabal</a>.


### Prerequisites

After installing haskell and cabal you may run
  ```sh
  cabal update
  ```
to update cabal


Install Alex
  ```sh
  cabal install alex
  ```
  
Install Happy
  ```sh
  cabal install happy
  ```

### Installation
1. Clone the repo
   ```sh
   git clone https://github.com/Andrei123f/MQL.git
   ```
2. Run the makefile
   ```sh
   make
   ```
3. Run the following command for running your program
   ```sh
   ./csvql myProgram.mql
   ```
   where myProgram.mql is your program written in the MQL language.

<p align="right">(<a href="#top">back to top</a>)</p>

<!-- USAGE EXAMPLES -->
## Usage
Anyone who wants a light and fast csv handling SQL language to manipulate CSV data.

<p align="right">(<a href="#top">back to top</a>)</p>

<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

If you have a suggestion that would make this better, please fork the repo and create a pull request. You can also simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE.txt` for more information.

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- CONTACT -->
## Contact

Andrei Popa - [@andreispkpd](https://twitter.com/andreispkpd) - andrei.popabd@gmail.com

Project Link: [https://github.com/Andrei123f/MQL](https://github.com/Andrei123f/MQL)

<p align="right">(<a href="#top">back to top</a>)</p>

<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/Andrei123f/MQL.svg?style=for-the-badge
[contributors-url]: https://github.com/Andrei123f/MQL/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/Andrei123f/MQL.svg?style=for-the-badge
[forks-url]: https://github.com/Andrei123f/MQL/network/members
[stars-shield]: https://img.shields.io/github/stars/Andrei123f/MQL.svg?style=for-the-badge
[stars-url]: https://github.com/Andrei123f/MQL/stargazers
[issues-shield]: https://img.shields.io/github/issues/Andrei123f/MQL.svg?style=for-the-badge
[issues-url]: https://github.com/Andrei123f/MQL/issues
[license-shield]: https://img.shields.io/github/license/Andrei123f/MQL.svg?style=for-the-badge
[license-url]: https://github.com/Andrei123f/MQL/blob/main/LICENSE.txt
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://www.linkedin.com/in/andrei-popa-563916192
[product-screenshot]: https://github.com/Andrei123f/MQL/blob/main/public/cosmin_try.png
