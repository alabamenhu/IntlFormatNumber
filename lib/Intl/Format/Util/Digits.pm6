sub EXPORT() {
     my %digits := %(
          adlm     => (my str @ = <𞥐 𞥑 𞥒 𞥓 𞥔 𞥕 𞥖 𞥗 𞥘 𞥙>),
          ahom     => (my str @ = <𑜰 𑜱 𑜲 𑜳 𑜴 𑜵 𑜶 𑜷 𑜸 𑜹>),
          arab     => (my str @ = <٠ ١ ٢ ٣ ٤ ٥ ٦ ٧ ٨ ٩>),
          arabext  => (my str @ = <۰ ۱ ۲ ۳ ۴ ۵ ۶ ۷ ۸ ۹>),
          bali     => (my str @ = <᭐ ᭑ ᭒ ᭓ ᭔ ᭕ ᭖ ᭗ ᭘ ᭙>),
          beng     => (my str @ = <০ ১ ২ ৩ ৪ ৫ ৬ ৭ ৮ ৯>),
          bhks     => (my str @ = <𑱐 𑱑 𑱒 𑱓 𑱔 𑱕 𑱖 𑱗 𑱘 𑱙>),
          brah     => (my str @ = <𑁦 𑁧 𑁨 𑁩 𑁪 𑁫 𑁬 𑁭 𑁮 𑁯>),
          cakm     => (my str @ = <𑄶 𑄷 𑄸 𑄹 𑄺 𑄻 𑄼 𑄽 𑄾 𑄿>),
          cham     => (my str @ = <꩐ ꩑ ꩒ ꩓ ꩔ ꩕ ꩖ ꩗ ꩘ ꩙>),
          deva     => (my str @ = <० १ २ ३ ४ ५ ६ ७ ८ ९>),
          diak     => (my str @ = <𑥐 𑥑 𑥒 𑥓 𑥔 𑥕 𑥖 𑥗 𑥘 𑥙>),
          fullwide => (my str @ = <０ １ ２ ３ ４ ５ ６ ７ ８ ９>),
          gong     => (my str @ = <𑶠 𑶡 𑶢 𑶣 𑶤 𑶥 𑶦 𑶧 𑶨 𑶩>),
          gonm     => (my str @ = <𑵐 𑵑 𑵒 𑵓 𑵔 𑵕 𑵖 𑵗 𑵘 𑵙>),
          gujr     => (my str @ = <૦ ૧ ૨ ૩ ૪ ૫ ૬ ૭ ૮ ૯>),
          guru     => (my str @ = <੦ ੧ ੨ ੩ ੪ ੫ ੬ ੭ ੮ ੯>),
          hanidec  => (my str @ = <〇 一 二 三 四 五 六 七 八 九>),
          hmng     => (my str @ = <𖭐 𖭑 𖭒 𖭓 𖭔 𖭕 𖭖 𖭗 𖭘 𖭙>),
          hmnp     => (my str @ = <𞅀 𞅁 𞅂 𞅃 𞅄 𞅅 𞅆 𞅇 𞅈 𞅉>),
          java     => (my str @ = <꧐ ꧑ ꧒ ꧓ ꧔ ꧕ ꧖ ꧗ ꧘ ꧙>),
          kali     => (my str @ = <꤀ ꤁ ꤂ ꤃ ꤄ ꤅ ꤆ ꤇ ꤈ ꤉>),
          khmr     => (my str @ = <០ ១ ២ ៣ ៤ ៥ ៦ ៧ ៨ ៩>),
          knda     => (my str @ = <೦ ೧ ೨ ೩ ೪ ೫ ೬ ೭ ೮ ೯>),
          lana     => (my str @ = <᪀ ᪁ ᪂ ᪃ ᪄ ᪅ ᪆ ᪇ ᪈ ᪉>),
          lanatham => (my str @ = <᪐ ᪑ ᪒ ᪓ ᪔ ᪕ ᪖ ᪗ ᪘ ᪙>),
          laoo     => (my str @ = <໐ ໑ ໒ ໓ ໔ ໕ ໖ ໗ ໘ ໙>),
          latn     => (my str @ = <0 1 2 3 4 5 6 7 8 9>),
          lepc     => (my str @ = <᱀ ᱁ ᱂ ᱃ ᱄ ᱅ ᱆ ᱇ ᱈ ᱉>),
          limb     => (my str @ = <᥆ ᥇ ᥈ ᥉ ᥊ ᥋ ᥌ ᥍ ᥎ ᥏>),
          mathbold => (my str @ = <𝟎 𝟏 𝟐 𝟑 𝟒 𝟓 𝟔 𝟕 𝟖 𝟗>),
          mathdbl  => (my str @ = <𝟘 𝟙 𝟚 𝟛 𝟜 𝟝 𝟞 𝟟 𝟠 𝟡>),
          mathmono => (my str @ = <𝟶 𝟷 𝟸 𝟹 𝟺 𝟻 𝟼 𝟽 𝟾 𝟿>),
          mathsanb => (my str @ = <𝟬 𝟭 𝟮 𝟯 𝟰 𝟱 𝟲 𝟳 𝟴 𝟵>),
          mathsans => (my str @ = <𝟢 𝟣 𝟤 𝟥 𝟦 𝟧 𝟨 𝟩 𝟪 𝟫>),
          mlym     => (my str @ = <൦ ൧ ൨ ൩ ൪ ൫ ൬ ൭ ൮ ൯>),
          modi     => (my str @ = <𑙐 𑙑 𑙒 𑙓 𑙔 𑙕 𑙖 𑙗 𑙘 𑙙>),
          mong     => (my str @ = <᠐ ᠑ ᠒ ᠓ ᠔ ᠕ ᠖ ᠗ ᠘ ᠙>),
          mroo     => (my str @ = <𖩠 𖩡 𖩢 𖩣 𖩤 𖩥 𖩦 𖩧 𖩨 𖩩>),
          mtei     => (my str @ = <꯰ ꯱ ꯲ ꯳ ꯴ ꯵ ꯶ ꯷ ꯸ ꯹>),
          mymr     => (my str @ = <၀ ၁ ၂ ၃ ၄ ၅ ၆ ၇ ၈ ၉>),
          mymrshan => (my str @ = <႐ ႑ ႒ ႓ ႔ ႕ ႖ ႗ ႘ ႙>),
          mymrtlng => (my str @ = <꧰ ꧱ ꧲ ꧳ ꧴ ꧵ ꧶ ꧷ ꧸ ꧹>),
          newa     => (my str @ = <𑑐 𑑑 𑑒 𑑓 𑑔 𑑕 𑑖 𑑗 𑑘 𑑙>),
          nkoo     => (my str @ = <߀ ߁ ߂ ߃ ߄ ߅ ߆ ߇ ߈ ߉>),
          olck     => (my str @ = <᱐ ᱑ ᱒ ᱓ ᱔ ᱕ ᱖ ᱗ ᱘ ᱙>),
          orya     => (my str @ = <୦ ୧ ୨ ୩ ୪ ୫ ୬ ୭ ୮ ୯>),
          osma     => (my str @ = <𐒠 𐒡 𐒢 𐒣 𐒤 𐒥 𐒦 𐒧 𐒨 𐒩>),
          rohg     => (my str @ = <𐴰 𐴱 𐴲 𐴳 𐴴 𐴵 𐴶 𐴷 𐴸 𐴹>),
          saur     => (my str @ = <꣐ ꣑ ꣒ ꣓ ꣔ ꣕ ꣖ ꣗ ꣘ ꣙>),
          segment  => (my str @ = <🯰 🯱 🯲 🯳 🯴 🯵 🯶 🯷 🯸 🯹>),
          shrd     => (my str @ = <𑇐 𑇑 𑇒 𑇓 𑇔 𑇕 𑇖 𑇗 𑇘 𑇙>),
          sind     => (my str @ = <𑋰 𑋱 𑋲 𑋳 𑋴 𑋵 𑋶 𑋷 𑋸 𑋹>),
          sinh     => (my str @ = <෦ ෧ ෨ ෩ ෪ ෫ ෬ ෭ ෮ ෯>),
          sora     => (my str @ = <𑃰 𑃱 𑃲 𑃳 𑃴 𑃵 𑃶 𑃷 𑃸 𑃹>),
          sund     => (my str @ = <᮰ ᮱ ᮲ ᮳ ᮴ ᮵ ᮶ ᮷ ᮸ ᮹>),
          takr     => (my str @ = <𑛀 𑛁 𑛂 𑛃 𑛄 𑛅 𑛆 𑛇 𑛈 𑛉>),
          talu     => (my str @ = <᧐ ᧑ ᧒ ᧓ ᧔ ᧕ ᧖ ᧗ ᧘ ᧙>),
          tamldec  => (my str @ = <௦ ௧ ௨ ௩ ௪ ௫ ௬ ௭ ௮ ௯>),
          telu     => (my str @ = <౦ ౧ ౨ ౩ ౪ ౫ ౬ ౭ ౮ ౯>),
          thai     => (my str @ = <๐ ๑ ๒ ๓ ๔ ๕ ๖ ๗ ๘ ๙>),
          tibt     => (my str @ = <༠ ༡ ༢ ༣ ༤ ༥ ༦ ༧ ༨ ༩>),
          tirh     => (my str @ = <𑓐 𑓑 𑓒 𑓓 𑓔 𑓕 𑓖 𑓗 𑓘 𑓙>),
          vaii     => (my str @ = <꘠ ꘡ ꘢ ꘣ ꘤ ꘥ ꘦ ꘧ ꘨ ꘩>),
          wara     => (my str @ = <𑣠 𑣡 𑣢 𑣣 𑣤 𑣥 𑣦 𑣧 𑣨 𑣩>),
          wcho     => (my str @ = <𞋰 𞋱 𞋲 𞋳 𞋴 𞋵 𞋶 𞋷 𞋸 𞋹>),
     );

     Map.new: '%digits' => %digits

}