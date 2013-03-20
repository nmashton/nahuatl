--# -path=.:../abstract:../common:../../prelude

concrete LexiconNah of Lexicon = open
	ParadigmsNah, ResNah in {

  flags
    optimize = values ;
    coding = utf8 ;

  lin
  		-- add_V3
	  	airplane_N = mkN "avión" ;
	  	-- alas_Interj
	  	-- already_Adv
	  	-- animal_N
		-- answer_V2S ;
	 	apartment_N = mkN "apartmento" ;
	  	apple_N = mkN "manzana" ;
		art_N = mkN "arte" ;
		ashes_N = mkN "nextli" ;
		-- ask_V2Q ;
	  	baby_N = mkN "pilli" TinRed ;
	  	-- back_N
	  	bad_A = mkA "acualli" Tin ;
	  	bank_N = mkN "banco" ;
	  	-- bark_N 
	  	beautiful_A = mkA "cualli" Tin ;
		-- become_VA
		beer_N = mkN "cerveza" ;
		-- beg_V2V
		belly_N = mkN "ìtitl" ;
	  	big_A = mkA "huēyi" ;
	  	bike_N = mkN "bicicleta" ;
	  	bird_N = mkN "tōtōtl" Meh ;
	  	-- bite_V2
	    black_A = mkA " tlīltic" ;
	    blood_N = mkN "eztli" ;
	    -- blow_V
		blue_A = mkA "matlactic" ;
		boat_N = mkN "ācalli" ;
		bone_N = mkN "omitl" ;
	  	book_N = mkN "āmoxtli" ;
		-- boot_N
		boss_N = mkN "tēcòtli" Meh ;
	  	-- boy_N = mkN "pilli" TinRed ; -- actually, child
	  	bread_N = mkN "pan" ;
	  	break_V2 = mkV3 "teyīnia" ;
	  	breast_N = mkN "chīchīhualli" ;
	  	-- breathe_V
		broad_A = mkA "pàpatlāhuac" ;
		-- brother_N2 = mkN2 (mkN masculine (mkN "brother")) (mkPrep "of") ;
	  	brown_A = mkA "chocolātic" ;
		butter_N = mkN "mantequilla" ;
		buy_V2 = mkV2 "cōhua" ;
		camera_N = mkN "cámara" ;
		cap_N = mkN "copilli" ;
		car_N = mkN "coche" ;
		carpet_N = mkN "alfombra" ;
	    cat_N = mkN "miztli" TinRed ;
		ceiling_N = mkN "techo" ;
		chair_N = mkN "cēhuilōni" ;
		cheese_N = mkN "queso" ;
		child_N = mkN "pilli" TinRed ;
		church_N = mkN "iglesia" ;
	  	city_N = mkN "āltepētl" ;
		clean_A = mkA "chipāhuac" ;
		-- clever_A = regADeg "clever" ;
		close_V2 = mkV2 "tzacua" ;
		cloud_N = mkN "mixtli" ;
		coat_N = mkN "capote" ;
		cold_A = mkA "cecēc" ;
		come_V = huItz_V ;
		computer_N = mkN "ordenador" ;
		-- correct_A
		-- country_N = mkN "country" ;
		count_V2 = mkV2 "pōhua" ;
		-- cousin_N = mkN human (mkN "cousin") ;
		cow_N = mkN "huācax" Meh ;
		cut_V2 = mkV2 "cotōna" ;
		day_N = mkN "ilhuitl" ;
		die_V = mkV "miqui" ;
		dig_V = mkV2 "tataca" ; --?
		dirty_A = mkA "catzāhuac" ;
	--  distance_N3 = mkN3 (mkN "distance") fromP toP ;
		doctor_N = mkN "tīcitl" GlotRed ;
		dog_N = mkN "chichi" Meh ;
		door_N = mkN "puerta" ;
		-- do_V2
		--  drink_V2 = dirV2 (irregV "drink" "drank" "drunk") ;
		dry_A = mkA "huāyic" ;
		dull_A = mkA "yacatepontic" ;
		dust_N = mkN "teuhtli" ;
		ear_N = mkN "nacaztli" ;--body part
		earth_N = mkN "tlālli" ;
	--  easy_A2V = mkA2V (regA "easy") forP ;
	--  eat_V2 = dirV2 (irregV "eat" "ate" "eaten") ;
		egg_N = mkN "tōtoltetl" ;
	--	empty_A = regADeg "empty" ;
		enemy_N = mkN "yāōtl" Glot ;
		eye_N = mkN "īxtli" ;
		factory_N = mkN "fábrica" ; -- f might cause problems?
		fall_V = mkV "huetzi" ;
		-- far_Adv
		--  father_N2 = mkN2 (mkN masculine (mkN "father")) (mkPrep "of") ;
		fat_N = mkN "ceceyotl" ; -- of an animal
		-- fear_VS
		fear_V2 = mkV2 "māhuilia" ;
		feather_N = mkN "quetzalli" ;
		fight_V2 = mkV2 "īxnāmiqui" ;
		find_V2 = mkV2 "nāmiqui" ;
		fingernail_N = mkN "iztetl" ;
		fire_N = mkN "tletl" ;
		fish_N = mkN "michin" Meh ;
		float_V = mkVDep "matoxoma" ;
		floor_N = mkN "suelo" ; --?
		flower_N = mkN "xōchitl" ;
		flow_V = mkV "mēya" ;
		fly_V = mkV "patlāni" ;
		fog_N = mkN "āyahuitl" ;
		foot_N = mkN "icxitl" ;
		forest_N = mkN "cuauhtlâ" ;
		forget_V2 = mkV2 "ilcāhua" ;
		freeze_V = "cehuetzi" ; --?
		fridge_N = mkN "refrigerador" ;
		friend_N = mkN "icnīuh" "icnīhuān" ;
		fruit_N = mkN "xocotl" ;
		-- full_A
	--  fun_AV = mkAV (regA "fun") ;
		garden_N = mkN "xōchichināncalli" ;
	--  girl_N = mkN feminine (mkN "girl") ;
		glove_N = mkN "māpīccatl" ;
	  	gold_N = mkN "teōcuitlatl" ;
	  	good_A = mkA "cualli" Tin ;
		go_V = yauh_V ;
		grammar_N = mkN "gramática" ;
		grass_N = mkN "zacatl" ;
		green_A = mkA "celic" ;
		guts_N = mkN "cuitlaxcolli" ;
		hair_N = mkN "tzontli" ;
		hand_N = mkN "māitl" ;
		harbour_N = mkN "puerto" ;
		hate_V2 = mkV2 "cocōlia" ;
		hat_N = mkN "sombrero" ;
	--  have_V2 = dirV2 (mk5V "have" "has" "had" "had" "having") ;
		head_N = mkN "cuāitl" ;
		heart_N = mkN "tlecuīlli" ;
		hear_V2 = mkV2 "caqui" ;
		heavy_A = mkA "etic" ;
		hill_N = mkN "tzacualli" ;
		hit_V2 = mkV2 "huītequi" ;
		-- hold_V2
	--  hope_VS = mkVS (regV "hope") ;
		horn_N = mkN "cuācuahuitl" ;
		horse_N = mkN "cahuayo" Meh ;
		hot_A = mkA "totōnqui" ;
		house_N = mkN "calli" ;
		-- hunt_V2
		husband_N = mkN "oquichtli" ;
		ice_N = mkN "cetl" ;
		--  important_A = compoundADeg (regA "important") ;
		industry_N = mkN "industria" ;
		iron_N = mkN "tepoztli" ;
		-- john_PN
		kill_V2 = mkV2 "mictia" ;
		king_N = mkN "tlàtoāni" ;
		knee_N = mkN "tlancuāitl" ;
		-- know_V2 = mkV2 "mati" ; -- quite irregular!
		-- know_VQ
		-- know_VS
		--  lake_N = mkN "lake" ;
		lamp_N = mkN "lámpara" ;
		language_N = mkN "tlàtōlli" ;
		laugh_V = mkV "huetzca" ;
		-- leaf_N
		--  learn_V2 = dirV2 (regV "learn") ;
		leather_N = mkN "cuetlaxtli" ;
		leave_V2 = mkV2 "cāhua" ;
		-- left_Ord
		leg_N = mkN "metztli" ;
		lie_V = mkV "iztlacati" ;
		--  like_V2 = dirV2 (regV "like") ;
		listen_V2 = mkV2 "caqui" ;
		liver_N = mkN "ēlli" ;
		live_V = mkV "nemi" ;
		--  long_A = regADeg "long" ;
		lose_V2 = mkV2 "poloa" ;
		louse_N = mkN "atemitl" ; -- strangely, inanimate?
		love_N = mkN "tlazòtlaliz" ;
		love_V2 = mkV "tlazòtla" ;
		man_N = mkN "oquichtli" Tin ;
		--  married_A2 = mkA2 (regA "married") toP ;
		meat_N = mkN "nacatl" ;
		milk_N = mkN "chīchīhualāyōtl" ;
		moon_N = mkN "mētztli" ;
		--  mother_N2 = mkN2 (mkN feminine (mkN "mother")) (mkPrep "of") ;
		mountain_N = mkN "tepētl" GlotRed ;
		mouth_N = mkN "camatl" ;
		music_N = mkN "cuīcatl" ;
		name_N = mkN "tōcāitl" ;
		narrow_A = mkA "pitzāhua" ;
		-- near_A -- this would use locative forms...
		neck_N = mkN "quechtli" ;
		new_A = mkA "yancuīc" ;
		newspaper_N = mkN "periódico" ;
		night_N = mkN "yohualli" ;
		nose_N = mkN "yacatl" ;
		-- now_Adv -- āxcān
		-- number_N
		oil_N = mkN "aceite" ;
		--	old_A = regADeg "old" ;
		open_V2 = mkV2 "tlapohua" ;
		--  paint_V2A = mkV2A (regV "paint") noPrep ;
		paper_N = mkN "āmatl" ;
		--  paris_PN = mkPN (mkN nonhuman (mkN "Paris")) ;
		--  peace_N = mkN "peace" ;
		pen_N = mkN "bolígrafo" ;
		person_N = mkN "tlācatl" Glot ;
		planet_N = mkN "planeta" ;
		plastic_N = mkN "plástico" ;
		--  play_V2 = dirV2 (regV "play") ;
		-- play_V
		policeman_N = mkN "policía" ;
		priest_N = mkN "padre" Meh ;
		--  probable_AS = mkAS (regA "probable") ;
		pull_V2 = mkV2 "tilāna" ;
		push_V2 = mkV2 "cuatopēhua" ;
		-- put_V2
		queen_N = mkN "reina" ;
		question_N = mkN "tlàtlanilli" ; --?
		radio_N = mkN "radio" ;
		--  rain_V0 = mkV0 (regV "rain") ;
		read_V2 = mkV2 "pōhua" ;
		-- ready_A
		-- reason_N
		red_A = mkA "chīchīltic" ;
		--  religion_N = mkN "religion" ;
		restaurant_N = mkN "restaurante" ;
		-- right_Ord
		river_N = mkN "ātoyatl" ;
		road_N = mkN "òtli" ;
		rock_N = mkN "tetl" ;
		--  roof_N = mkN "roof" ;
		rope_N = mkN "mecatl" ;
		rotten_A = mkA "popoyōtic" ;
		rubber_N = mkN "ōlli" ;
		-- rub_V2
		rule_N = mkN "nahuatīlli" ;
		run_V = mkVDep "tlaloa" ;
		salt_N = mkN "iztatl" ;
		sand_N = mkN "xālli" ;
		--  say_VS = mkVS (irregV "say" "said" "said") ;
		school_N = mkN "tēlpōchcalli" ;
		science_N = mkN "tlàmatiliztli" ; -- "knowledge"
		scratch_V2 = mkV2 "huahuana" ;
		--  sea_N = mkN "sea" ;
		seed_N = mkN "achtli" ;
		seek_V2 = mkV2 "tēmoa" ;
		see_V2 = mkV2 "itta" ; -- might need some attention!
		--  sell_V3 = dirV3 (irregV "sell" "sold" "sold") toP ;
		--  send_V3 = dirV3 (irregV "send" "sent" "sent") toP ;
		sew_V = mkV2 "ìtzoma" ;
		sharp_A = mkA "yacahuitztic" ;
		sheep_N = mkN "ichcatl" Meh ;
		ship_N = mkN "ācalli" ;
		shirt_N = mkN "camisa" ;
		shoe_N = mkN "cactli" ;
		--  shop_N = mkN "shop" ;
		--  short_A = regADeg "short" ;
		silver_N = mkN "teōcuitlatl" ;
		sing_V = mkV "cuīca" ;
		--  sister_N = mkN2 (mkN feminine (mkN "sister")) (mkPrep "of") ;
		sit_V = mkVDep "tlālia" ;
		skin_N = mkN "cuetlaxtli" ; -- also "leather"
		sky_N = mkN "ilhuicatl" ;
	  	sleep_V = mkV "cochi" ;
		small_A = mkA "tepitōn" "tepitotōn" ;
		-- smell_V
		smoke_N = mkN "pōctli" ;
		smooth_A = mkA "ahuiāc" ;
		snake_N = mkN "cōhuātl" GlotRed ;
		snow_N = mkN "cepayahuitl" ;
		sock_N = mkN "calcetín" ;
		song_N = mkN "cuīcatl" ;
		speak_V2 = mkV2 "ìtoa" ; -- "speak of"
		spit_V = mkV "chìcha" ; -- ?
		split_V2 = mkV2 "xelihui" ;
		-- squeeze_V2
		stab_V2 = mkV2 "zo" ; -- monosyllabic ... does it work?
		stand_V = mkV "ìcac" ; -- looks dubious
		star_N = mkN "citlālin" TinRed ;
		steel_N = mkN "acero" ;
		stick_N = mkN "cuahuitl" ;
		stone_N = mkN "tetl" ;
		stop_V = mkVDep "cāhua" ; -- does this work, semantically?
		stove_N = mkN "cocina" ;
		straight_A = mkA "melāhuac" ;
		--  student_N = mkN human (mkN "student") ;
		stupid_A = mkA "xolopìtli" ;
		suck_V2 = mkV2 "chichīna" ;
		sun_N = mkN "tōnatiuh" ;
		swell_V = mkV "tēmi" ; -- "to fill up"
		swim_V = mkV "ācui" ;
		--  switch8off_V2 = dirV2 (partV (regV "switch") "off") ;
		--  switch8on_V2 = dirV2 (partV (regV "switch") "on") ;
		table_N = mkN "mesa" ;
		tail_N = mkN "cuitlapilli" ;
		--  talk_V3 = mkV3 (regV "talk") toP aboutP ;
		teacher_N = mkN "tēmachtìqui" ;
		teach_V2 = mkV2 "machtia" ; -- "this verb has peculiarities of its own (§19.5)"
		television_N = mkN "televisión" ;
		--  thick_A = regADeg "thick" ;
		thin_A = mkA "canāhuac" ;
		-- think_V
		throw_V2 = mkV2 "tlāza" ;
		tie_V2 = mkV2 "ilpia" ;
		-- today_Adv -- āxcān ;
		tongue_N = mkN "nenpilli" ;
		tooth_N = mkN "tlantli" ;
		train_N = mkN "tren" ;
		travel_V = mkV "nènemi" ;
		tree_N = mkN "cuahuitl" ;
		turn_V = mkVDep "cuepa" ;
		--  ugly_A = regADeg "ugly" ;
		uncertain_A = mkA "tetzotzon" ; -- looks ... uncertain
		understand_V2 = mkV2 "caqui" ;
		university_N = mkN "universidad" ;
		village_N = mkN "āltepētl" ;
		vomit_V = mkVDep "ìzotla" ;
		wait_V2 = mkV2 "chiya" ;
		-- walk_V = (regV "walk") ;
		--  warm_A = regADeg "warm" ;
		war_N = mkN "yāōyōtl" ;
		wash_V2 = mkV2 "pāca" ;
		watch_V2 = mkV2 "chiya" ;
		water_N = mkN "ātl" ;
		-- wet_A
		white_A = mkA "iztāc" ;
		wide_A = mkA "patlāhuac" ;
		wife_N = mkN "cihuātl" Glot ;
		wind_N = mkN "èecatl" ;
		window_N = mkN "pōchquiyāhuatl" ;
		wine_N = mkN "octli" ;
		wing_N = mkN "àtlapalli" ;
		win_V2 = mkV2 "tlāni" ;
		-- wipe_V2
		woman_N = mkN "cihuātl" Glot ;
		--  wonder_VQ = mkVQ (regV "wonder") ;
		wood_N = mkN "cuahuitl" ;
		worm_N = mkN "ocuilin" "ocuilimê" ;
		write_V2 = mkV2 "ìcuiloa" ;
		year_N = mkN "xihuitl" ;
		yellow_A = mkA "cōztic" ;
		--  young_A = regADeg "young" ;

}