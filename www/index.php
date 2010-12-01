
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title>
<?php echo $group_name; ?>
</title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr>
<td>
<a href="http://r-forge.r-project.org/">
<img src="http://<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->
<p class="c1 c4">
<span class="c0">EcoHydrology, An R package of fundamental ecohydrological models with data gathering and processing functions.</span>
</p>
<p class="c2">
<a name="h.rnlolc-gtoxmv">
</a>
<span class="c0">submitted to be considered for:</span>
</p>
<p class="c2">
<a name="h.g5136a-wuvxbt">
</a>
<span class="c0">Thematic Issue&rsquo; of the Environmental Modelling &amp; Software on &quot;The Future of Integrated Modelling Science and Technology&quot; </span>
</p>
<p class="c1 c4">
<span class="c0">ciem2010@iemss.org</span>
</p>
<p class="c2">
<a name="h.g5136a-wuvxbt">
</a>
<span class="c0">(Nov. 30, abstract request)</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Hydrological and ecological models are often used to analyze complex environmental phenomena. Unfortunately, the utility of these models is often limited by the fact that: (i) sub-process models are hidden within compiled programs, (ii) they are available only on computational platforms outside the economic reach of many students in developing countries, (iii) many models interface with and/or are implemented through proprietary software packages and (iv) many traditional models suffer from delayed software distribution and maintenance bottlenecks. This, in addition to the dilemma that compiled models do not allow the modeler to see solutions to sub-processes that have no intermediate output, renders them almost irrelevant in all but the most controlled of settings. For instance, models that run as pre-complied programs leave little flexibility for users to modify the model structure, and require considerable knowledge of the model underpinnings to understand how the process is being modeled. Thus, there is a need in the environmental modeling community to develop a general-purpose, open-access library of fundamental equations that describe the complex interactions between hydrology and ecology (commonly referred to as ecohydrology). We propose a library, written in the open-source programming language R, that provides an easily accessible suite of subroutines which simulate fundamental eco-hydrological processes. These subroutines can be combined to create more complex models of spatially and temporally varying environmental systems. There are minimal computational requirements for running R. Moreover, if local installation is not possible, there is a simple web interface for publicly available R web servers. </span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">The EcoHydrology R-packaged library is an offshoot from a long history of hydrological courses taught at Cornell University. It includes hydrologic models covering techniques ranging from the simple Thornthwaite-Mather Water Balance (a soil water budget model) to more complex energy budget based models. The package also includes sub-models of carbon dynamics, nitrogen cycles, plant growth modeling, and groundwater flows. The energy budget models are further developed to provide several solutions for calculating evapotranspiration, snow accumulation and melt. We also provide functions to remotely import and parse data from various sources, including meteorological data from the National Oceanic and Atmospheric Administration (NOAA) and steamflow data from the United States Geological Survey (USGS). These import and parsing functions are easily modified to facilitate data import and formatting from many other sources. </span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">The EcoHydrology package is fully documented, including detailed references to peer reviewed journal publications, which can be accessed online or from the modeling command prompt in the R interface. The documentation includes examples of simple model formulations, as well as examples of common data format importation from simple comma separated files to global datasets like the National Climatic Data Center&rsquo;s Global Summary of Day (GSOD). This allows a user to rapidly integrate the package and its functions into any current work, regardless of their comfort level with R. On the R-Forge central development platform we provide real-time updates of any model changes or implementation of the model. The platform includes public forums, a source code repository, feature requests and bug tracking. </span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">As such, this package provides a flexible foundation for scientists, engineers, and policy makers to base teaching exercises as well as for more applied use to model complex eco-hydrological interactions. Researchers can also easily integrate the library&rsquo;s modules into their own computational tools. As an added benefit, the package can serve as an important decision-support tool for resource managers and policy makers who have been needlessly constrained by manual spreadsheet models. Because R is free and openly supported on all computational platforms, this library is uniquely well-suited to academic and research fields where budgets for more expensive proprietary learning environments do not exist. &nbsp;</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Bringing the EcoHydrology package into the R language and joining with the other spatial- and geo-statistical packages available in the Comprehensive R Archive Network (CRAN) repositories creates a robust framework. This will enhance our ability to analyze environmental and hydrologic processes, contribute to the advancement of open-source hydrologic modeling, and help eliminate some of the limitations associated with traditional modeling approaches.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">References currently included:</span>
</p>
<p class="c1">
<span class="c0">Anderson, E.A., 1968. Development and testing of snow pack energy balance equations. Water Resour. Res. 4, 19-37.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Anderson, E.A., 1976. A Point Energy and Mass Balance Model of a Snow Cover. U.S. Gov. Print. Off., Washington.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Ardia, David and Mullen, Katharine, 2009. DEoptim: Differential Evolution Optimization in R. R package version 2.0-3. 2010, 1. Available at: </span>
<span class="c3">
<a href="http://www.google.com/url?q=http%3A%2F%2Fcran.r-project.org%2Fpackage%3DDEoptim&amp;sa=D&amp;sntz=1&amp;usg=AFQjCNGiBLQwqNvB3K27gcCZ0Q48QMWDnw">http://CRAN.R-project.org/package=DEoptim</a>
</span>
<span class="c0">. Accessed 09/03, 2010.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Arnold, J.G., and N. Fohrer. 2005. SWAT2000: current capabilities and research opportunities in applied watershed modelling. Hydrological Processes 19:563-572.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Beven, K.J. 1997. TOPMODEL: A critique. Hydrological Processes 11:1069-1085.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Beven, K.J., 2001. Rainfall-Runoff Modelling : The Primer. J. Wiley, Chichester, UK; New York.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Bristow, K.L., and Campbell, G.S., 1984. On the relationship between incoming solar radiation and daily maximum and minimum temperature. Agric. For. Meteorol. 31, 159-166.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Campbell, G.S., Norman, J.M., 2000. An Introduction to Environmental Biophysics. Springer, New York, NY [u.a].</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Corwin, D.L., Hopmans, J. and de Rooij, G.H. &nbsp;From field-to landscape-scale vadose zone processes: scale issues, Vadose Zone J. 5 (2006), pp. 129&ndash;139.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Fuka, D.R., 1995. SPAW nitrogen fate routine : development and corroboration. Thesis (M.S.)--Washington State University, 1995.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Garnier, B., Ohmura, A., 1968. A Method of Calculating the Direct Shortwave Radiation Income of Slopes. J. Appl. Meteorol. 7, 796-800.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Hornik, K., 2010. The R FAQ.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Ihaka, R., Gentleman, R., 1996. R: A language for data analysis and graphics. Journal of computational and graphical statistics 5, 299-314.</span>
</p>
<p class="c1">
<span class="c0">Jensen, M., Burman, R., Allen, R., 1990. Evapotranspiration and irrigation water requirements.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Jensen, M.E., Burman, R.D., Allen, R.G., American Society of Civil Engineers. Committee on </span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Irrigation Water Requirements., 1990. Evapotranspiration and Irrigation Water Requirements : A Manual. The Society, New York, N.Y.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Monteith, J.L., Unsworth, M.H., 2008. Principles of Environmental Physics. Elsevier, Amsterdam [etc.].</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Ndlovu, L.S., 1994. Weather data generation and its use in estimating evapotranspiration. Thesis (Ph. D.)--Washington State University, 1994.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Penman, H., 1948. Natural evaporation from open water, bare soil and grass. Proceedings of the Royal Society of London.Series A, Mathematical and Physical Sciences 193, 120-145.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Priestley, C., Taylor, R., 1972. On the assessment of surface heat flux and evaporation using large-scale parameters. Mon. Weather Rev. 100, 81-92.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Schneiderman, E.M. 2005. GWLF, In D. Radcliffe and M. Cabrera, eds. Modeling phosphorus in the environment. CRC Press.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Thornton, P.E., Hasenauer, H., White, M.A., 2000. Simultaneous estimation of daily solar radiation and humidity from observed temperature and precipitation: an application over complex terrain in Austria. Agric. For. Meteorol. 104, 255-271.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Walter, M.T., Brooks, E.S., McCool, D.K., King, L.G., Molnau, M., Boll, J., 2005. Process-based snowmelt modeling: does it require more input data than temperature-index modeling? Journal of Hydrology 300, 65-75.</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Wigmosta, M.S., B. Nijssen, P. Storck, and D.P. Lettenmaier, 2002. The Distributed Hydrology Soil Vegetation Model, In Mathematical Models of Small Watershed Hydrology and Applications, V.P. Singh, D.K. Frevert, eds., Water Resource Publications, Littleton, CO., p. 7-42.</span>
</p>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">
<strong>here</strong>
</a>. </p>
<p class="c1">
<span class="c0">Daniel R. Fuka</span>
</p>
<p class="c1">
<span class="c0">Dept. of Biological and Environmental Engineering, Cornell University, Ithaca, NY, USA</span>
</p>
<p class="c1">
<span class="c0">drf28@cornell.edu</span>
</p>
<p class="c1">
<span class="c0"> </span>
</p>
<p class="c1">
<span class="c0">Zachary M. Easton, </span>
</p>
<p class="c1">
<span class="c0">Dept. of Biological Systems Engineering, Blacksburg, VA, USA (after April 25th)</span>
</p>
<p class="c1">
<span class="c0">zme2@cornell.edu</span>
</p>
<p class="c1">
<span class="c0"> </span>
</p>
<p class="c1">
<span class="c0">Josephine Archibald</span>
</p>
<p class="c1">
<span class="c0">Dept. of Biological and Environmental Engineering, Cornell University, Ithaca, NY, USA </span>
<span class="c3">
<a href="mailto:jaa78@cornell.edu">jaa78@cornell.edu</a>
</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Erin S. Brooks</span>
</p>
<p class="c1">
<span class="c0">Department of Biological and Agricultural Engineering, University of Idaho, Moscow, ID, USA</span>
</p>
<p class="c1">
<span class="c0">ebrooks@uidaho.edu </span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Brian P. Buchanan</span>
</p>
<p class="c1">
<span class="c0">Dept. of Natural Resources, Cornell University, Ithaca, NY, USA</span>
</p>
<p class="c1">
<span class="c3">
<a href="mailto:bb386@cornell.edu">bb386@cornell.edu</a>
</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Asha N. Sharma</span>
</p>
<p class="c1">
<span class="c0">Dept. of Biological and Environmental Engineering, Cornell University, Ithaca, NY, USA</span>
</p>
<p class="c1">
<span class="c0">ans62@cornell.edu</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Vania R. Pereira</span>
</p>
<p class="c1">
<span class="c0">Instituto de Geociencias, Universidade Estadual de Campinas, Campinas, S&atilde;o Paulo, Brazil, CEP</span>
</p>
<p class="c1">
<span class="c0">vaniarp@ige.unicamp.br</span>
</p>
<p class="c1">
<span class="c0">&nbsp;</span>
</p>
<p class="c1">
<span class="c0">Tammo S. Steenhuis</span>
</p>
<p class="c1">
<span class="c0">Dept. of Biological and Environmental Engineering, Cornell University, Ithaca, NY, USA tss1@cornell.edu</span>
</p>
<p class="c1">
<span class="c0"> &nbsp;</span>
</p>
<p class="c1">
<span class="c0">M.Todd Walter</span>
</p>
<p class="c1">
<span class="c0">Dept. of Biological and Environmental Engineering, Cornell University, Ithaca, NY, USA</span>
</p>
<p class="c1">
<span class="c3">
<a href="mailto:mtw5@cornell.edu">mtw5@cornell.edu</a>
</span>
</p>
</body>
</html>
