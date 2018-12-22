
var page = require('webpage').create();
page.open('https://www.mgm.gov.tr/tahmin/il-ve-ilceler.aspx?il=Ankara&ilce=Yenimahalle', function(status){
     if (status == 'success') {
         var current = page.evaluate(function () {
             var sicaklik = document.getElementById('stn1').innerText;
             var nem = document.querySelector('[ng-bind="sondurum[0].nem"]').innerText;
             var ruzgar = document.querySelector('[ng-bind="sondurum[0].ruzgarHiz|number:0"]').innerText;
             var intervalsStart = document.querySelectorAll('[ng-bind="item.tarihOnceki | meteorDateFormatSaat"]');
             var intervalsEnd = document.querySelectorAll('[ng-bind="item.tarih | meteorDateFormatSaat"]');
             var days = document.querySelectorAll('[ng-bind="item.tarihOnceki | meteorDateFormatGun"]');
             var stats = document.querySelectorAll('[ng-src^="../Images_Sys/hadiseler/"]');
             var hourlySicaklik = document.querySelectorAll('[ng-bind="item.hissedilenSicaklik | kaliteKontrol"]');
             return [sicaklik, nem, ruzgar, intervalsStart, intervalsEnd, days, stats, hourlySicaklik];
         });

         var [sicaklik, nem, ruzgar, intervalsStart, intervalsEnd, days, stats, hourlySicaklik] = current;
         console.log('Sicaklik => ' + sicaklik.replace(/\s\s+/g, ' '));
         console.log('Nem      => ' + '%' + nem);
         console.log('Ruzgar   => ' + ruzgar + ' km/s');
         console.log('');

         intervalsStart.forEach(function(start, i) {
             console.log(days[i].innerText + " :: "
                          + start.innerText + " - " + intervalsEnd[i].innerText + " => "
                          + hourlySicaklik[i].innerText + " °C, "
                          + stats[i+1].getAttribute("title"))
         });
     } else {
         console.log('maalesef...');
     }
     page.close();
     phantom.exit();
})


