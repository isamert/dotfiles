var page = require('webpage').create();
page.open('https://www.mgm.gov.tr/tahmin/il-ve-ilceler.aspx?il=Ankara&ilce=Yenimahalle', function(status){
     if (status == 'success') {
         var current = page.evaluate(function () {
             var sicaklik = document.getElementById('stn1').innerText
             var nem = document.querySelector('[ng-bind="sondurum[0].nem"]').innerText;
             var ruzgar = document.querySelector('[ng-bind="sondurum[0].ruzgarHiz|number:0"]').innerText
             return [sicaklik, nem, ruzgar];
         });

         var [sicaklik, nem, ruzgar] = current;
         console.log('Sicaklik: ' + sicaklik);
         console.log('Nem     : ' + '%' + nem);
         console.log('Ruzgar  : ' + ruzgar + ' km/s');
     }
     else {
         console.log('maalesef...');
     }
     page.close();
     phantom.exit();
})
