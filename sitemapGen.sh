#!/bin/bash
#
#Generates the sitemap.xml file required by indexers.

if [ -d "doc" ]
then
   echo "Remove folder doc to avoid duplicates. Execution stopped."
   exit
fi

SITEMAPFILE=sitemap.xml
ROOTSITE='https://ecasglez.github.io/FortranUtilities/'
GOOGLEFILE='google486af277c416d0f7.html'
GITBRANCH=`git rev-parse --abbrev-ref HEAD` # git branch --show-current #with git 2.22 or above

if [[ ${GITBRANCH} != "gh-pages" ]]
then
   echo "This tool must be run on git branch gh-pages. Execution stopped."
   exit
fi

echo '<?xml version="1.0" encoding="UTF-8"?>' > ${SITEMAPFILE}
echo '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">' >> ${SITEMAPFILE}

for F in `find -name '*.html'`
do
   FILE=`echo ${F} | cut -d/ -f2-`
   if [[ ${FILE} == ${GOOGLEFILE} ]]
   then
      continue
   fi
   echo '   <url>' >> ${SITEMAPFILE}
   echo '      <loc>'${ROOTSITE}${FILE}'</loc>' >> ${SITEMAPFILE}
   echo '   </url>' >> ${SITEMAPFILE}
done

echo '</urlset>' >> ${SITEMAPFILE}







