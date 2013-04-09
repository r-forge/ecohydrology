The SW_sandbox folder is a repository of useful R code for sharing between members of the 
Soil and Water group at Cornell University,and any other developers interested in sharing
code relevant to Ecohydrology.

Started:  April 2013


Please name your files as descriptively as possible so people know what can be found within.
Also, include your name, contact info, addition description, and any other useful info at the top of all files.


The file structure:  (Please update)

Examples
- Includes unpolished code that can be used as an example for others to work through

Functions_In_Progress
- Code has been made into functions
- Major changes/updates are planned
- Might include extraneous lines of code

Functions
- Code here should be directly executable using source() function
- No extraneous lines of code



Changes/additions are welcome and encouraged to the file structure, as well as files within. 



INSTRUCTIONS ACCESSING AND UPDATING FILES from the SW_sandbox:

Version control options:  
TortoiseSVM    http://tortoisesvn.net/
Learn about version control: http://www.software-carpentry.org/4_0/vc/intro.html       
(They suggest a different VSN software, also fine)


Using TortoiseSVN to grab files
Once you have downloaded and installed an SVN provider, you can grab the data from the R-forge website.  
- Create a new folder in an easy-to-find place. 
- Open the folder and right click.  Chose SVN Checkout
- You will need to change the path in the first box.  
-  " svn checkout svn+ssh://PUT_YOUR_ID_here@scm.r-forge.r-project.org/svnroot/ecohydrology/SW_sandbox "
-  (You will have to enter your developer password a few times..)
- OR TO CHECK IT OUT ANONYMOUSLY : " svn checkout svn://scm.r-forge.r-project.org/svnroot/ecohydrology/SW_sandbox "
- Notice that now your new folder will have a bunch of new files in it with little green check icons over the folders.  
- This means that you have the newest version of these files


Updating your changes to the repository.
Once you have made some changes to any of the files, 
you will want to update the repository so that other people can access these files as well. 
- Right click on the folder that contains all your changes. 
- Click SVN Update (to make sure your changes are compatible/made on the newest version of the files)
- Then click SVN Commit to make your changes available to everyone else
- Don't forget to include a comment that describes the changes you made
