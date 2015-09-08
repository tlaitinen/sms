function __(k,d) {
    var strings = {
        'textmessageform.title' : 'Lähetettävän tekstiviestin muokkaus',
        'text' : 'Teksti',
        'textEditor' : 'Teksti',
        'textmessageform.abort' : 'Keskeytä tekstiviestin lähetys',
        'textmessageform.send' : 'Lähetä tekstiviesti vastaanottajille',
        'textmessageform.replyToText' : 'Vastaus viestiin',
        'receivedtextmessageform.title' : 'Vastaanotettu tekstiviesti',
        'receivedtextmessageform.reply' : 'Vastaa tekstiviestiin',
        'textmessageform.length' : 'Tekstiviestin pituus',
        'textmessageform.lengthMessage' : '{0} merkkiä, {1} viestiä, {2} merkkiä per viesti. Merkkejä jäljellä: {3}',
        'textmessagesgrid.title' : 'Tekstiviestit',
        'textmessagesgrid.sender' : 'Lähettäjä',
        'textmessagesgrid.new' : 'Uusi tekstiviesti',
        'textmessagesgrid.remove' : 'Poista valitut tekstiviestit',
        'textmessagesgrid.emptyPaging' : 'Ei tekstiviestejä',
        'textmessagerecipientsgrid.emptyPaging' : 'Ei vastaanottajia',
        'textmessagerecipientsgrid.title' : 'Viestin vastaanottajat',
        'queued' : 'Lisätty jonoon',
        'sent' : 'Lähetetty',
        'delivered' : 'Toimitettu',
        'textmessages' : 'Tekstiviestit',
        'rowedit.save' : 'Tallenna',
        'rowedit.cancel' : 'Peruuta',
        'phone' : 'Puhelinnumero',
        'dateOfBirth' : 'Syntymäaika',
        'card' : 'Kortti',
        'allowSms' : 'SMS',
        'allowEmail' : '@',
        'clientsgrid.title' : 'Kanta-asiakkaat',
        'clientsgrid.emptyPaging' : 'Ei kanta-asiakkaita',
        'clientsgrid.add' : 'Uusi kanta-asiakas',
        'clientsgrid.remove' : 'Poista valitut kanta-asiakkaat',
        'clientsgrid.sendMessage' : 'Lähetä viesti listan kanta-asiakkaille',
        'preview' : 'Esikatselu',
        'preview.title' : 'Esikatselu (voit sulkea ikkunan klikkaamalla kuvaa)',
        'previewFileId' : 'preview',
        'saveError.title': 'Tietojen tallentaminen epäonnistui',
        'saveError.message': 'Tietojen tallentaminen epäonnistui: ',
        'userpasswordform.title' : 'Käyttäjän salasanan muuttaminen',
        'ok' : 'OK',
        'cancel' : 'Peruuta',
        'passwordMinLength' : 'Salasanan tulee olla vähintään kuusi merkkiä pitkä',
        'validationError.title' : 'Virheellisiä tietoja',
        'validationError.message' : 'Lomakkeella on virheellisiä tietoja.',
        'validationError.password' : 'Salasanat eivät täsmää.',
        'login.title' : 'Kirjautuminen kanta-asiakasrekisteriin',
        'login.waittitle' : 'Kirjautuminen käynnissä',
        'login.waitmessage' : 'Lähetetään tietoja',
        'login.failedtitle' : 'Virhe',
        'login.failedmessage' : 'Kirjautuminen epäonnistui',
        'username' : 'Käyttäjänimi',
        'password' : 'Salasana',
        'login.login' : 'Kirjaudu sisään',
        'maintab.signout' : 'Kirjaudu ulos',
        'clients' : 'Kanta-asiakkaat',
        'instructions' : 'Ohje',
        'users' : 'Käyttäjät',
        'usersgrid.title' : 'Käyttäjät',
        'usersgrid.emptyPaging' : 'Ei käyttäjiä',
        'usersgrid.new' : 'Uusi käyttäjä',
        'usersgrid.remove' : 'Poista valitut käyttäjät',
        'usergroupscombo.emptyText' : 'Käyttäjäryhmä',
        'usergroupsgrid.title' : 'Käyttäjäryhmät',
        'usergroupsgrid.emptyPaging' : 'Ei käyttäjäryhmiä',
        'usergroupsgrid.new' : 'Uusi ryhmä',
        'usergroupsgrid.remove' : 'Poista valitut ryhmät',
        'userform.title' : 'Käyttäjän tietojen muokkaaminen',
        'userform.defaultUserGroupId' : 'Oletusryhmä',
        'userform.setUserPassword' : 'Vaihda salasana',
        'search' : 'Haku',
        'name' : 'Nimi',
        'firstName' : 'Etunimi',
        'lastName' : 'Sukunimi',
        'email' : 'Sähköpostiosoite',
        'timeZone' : 'Aikavyöhyke',
        'passwordAgain' : 'Salasana uudestaan',
        'userName' : 'Käyttäjä',
        'userGroupName' : 'Käyttäjäryhmä',
        'contentType' : 'Tiedostotyyppi',
        'insertionTime' : 'Ladattu',
        'save' : 'Tallenna muutokset',
        'saveandclose' : 'Tallenna muutokset ja sulje',
        'close' : 'Sulje',
        'closewithoutsaving' : 'Sulje tallentamatta muutoksia',
        'usergroupitemsgrid.title' : 'Oikeudet käyttäjäryhmissä',
        'usergroupitemsgrid.emptyPaging' : 'Ei oikeuksia käyttäjäryhmissä',
        'usergroupitemsgrid.remove' : 'Poista valitut käyttäjäoikeudet',
        'users.addReadPerm' : 'Lisää lukuoikeus valituille käyttäjille valittuihin ryhmiin',
        'users.addWritePerm' : 'Lisää luku- ja kirjoitusoikeus valituille käyttäjille valittuihin ryhmiin',
        'smsgrid.fileName' : 'Tiedosto',
        'amount' : 'Summa',
        'insertionTime' : 'Lisätty',
        'upload.title' : 'Tiedostojen lataus',
        'upload.button' : 'Lisää tositteita tiputtamalla tiedostoja tähän.',
        'upload.uploading' : 'Ladataan...',
        'usergroupform.title' : 'Käyttäjäryhmän tietojen muokkaaminen',
        'usergroupform.createPeriods' : 'Luotavien kirjanpitokuukausien lkm.',
        'emptyMonthCombo' : 'Suodata lista syntymäkuukauden perusteella',
        'month1' : 'Tammikuu',
        'month2' : 'Helmikuu',
        'month3' : 'Maaliskuu',
        'month4' : 'Huhtikuu',
        'month5' : 'Toukokuu',
        'month6' : 'Kesäkuu',
        'month7' : 'Heinäkuu',
        'month8' : 'Elokuu',
        'month9' : 'Syyskuu',
        'month10': 'Lokakuu',
        'month11': 'Marraskuu',
        'month12' : 'Joulukuu'
    };
    var parts = k.split('.');
    if (k in strings) {
        while (k in strings) {
            var pk = k;
            k = strings[k];
            if (pk == k)
                break;
        }
        return k;
    } else if (parts[parts.length - 1] in strings) {
        k = parts[parts.length - 1];
        while (k in strings) {
            var pk = k;
            k = strings[k];
            if (pk == k)
                break;
        }
        return k;
    } else if (d) {
        return __(d);
    } else {
        return k;
    }
};
