
-- HARD
-- source: https://platform.stratascratch.com/coding-question?id=10165&python=
-- create table

CREATE TABLE google_gmail_emails(
     id 	    VARCHAR(3),
     from_user 	VARCHAR(30),
     to_user 	VARCHAR(30),
     day 	    VARCHAR(2)
);

-- insert values

INSERT INTO google_gmail_emails( id, from_user, to_user, day)
VALUES
    ('0', '6edf0be4b2267df1fa', '75d295377a46f83236', '10'),
    ('1', '6edf0be4b2267df1fa', '32ded68d89443e808', '6'),
    ('2', '6edf0be4b2267df1fa', '55e60cfcc9dc49c17e', '10'),
    ('3', '6edf0be4b2267df1fa', 'e0e0defbb9ec47f6f7', '6'),
    ('4', '6edf0be4b2267df1fa', '47be2887786891367e', '1'),
    ('5', '6edf0be4b2267df1fa', '2813e59cf6c1ff698e', '6'),
    ('6', '6edf0be4b2267df1fa', 'a84065b7933ad01019', '8'),
    ('7', '6edf0be4b2267df1fa', '850badf89ed8f06854', '1'),
    ('8', '6edf0be4b2267df1fa', '6b503743a13d778200', '1'),
    ('9', '6edf0be4b2267df1fa', 'd63386c884aeb9f71d', '3'),
    ('10', '6edf0be4b2267df1fa', '5b8754928306a18b68', '2'),
    ('11', '6edf0be4b2267df1fa', '6edf0be4b2267df1fa', '8'),
    ('12', '6edf0be4b2267df1fa', '406539987dd9b679c0', '9'),
    ('13', '6edf0be4b2267df1fa', '114bafadff2d882864', '5'),
    ('14', '6edf0be4b2267df1fa', '157e3e9278e32aba3e', '2'),
    ('15', '75d295377a46f83236', '75d295377a46f83236', '6'),
    ('16', '75d295377a46f83236', 'd63386c884aeb9f71d', '8'),
    ('17', '75d295377a46f83236', '55e60cfcc9dc49c17e', '3'),
    ('18', '75d295377a46f83236', '47be2887786891367e', '10'),
    ('19', '75d295377a46f83236', '5b8754928306a18b68', '10'),
    ('20', '75d295377a46f83236', '850badf89ed8f06854', '7'),
    ('21', '75d295377a46f83236', '5eff3a5bfc0687351e', '2'),
    ('22', '75d295377a46f83236', '5dc768b2f067c56f77', '8'),
    ('23', '75d295377a46f83236', '114bafadff2d882864', '3'),
    ('24', '75d295377a46f83236', 'e0e0defbb9ec47f6f7', '3'),
    ('25', '75d295377a46f83236', '7cfe354d9a64bf8173', '10'),
    ('26', '5dc768b2f067c56f77', '114bafadff2d882864', '3'),
    ('27', '5dc768b2f067c56f77', '2813e59cf6c1ff698e', '5'),
    ('28', '5dc768b2f067c56f77', '91f59516cb9dee1e88', '6'),
    ('29', '5dc768b2f067c56f77', '5b8754928306a18b68', '6'),
    ('30', '5dc768b2f067c56f77', '6b503743a13d778200', '5'),
    ('31', '5dc768b2f067c56f77', 'aa0bd72b729fab6e9e', '10'),
    ('32', '5dc768b2f067c56f77', '850badf89ed8f06854', '1'),
    ('33', '5dc768b2f067c56f77', '406539987dd9b679c0', '7'),
    ('34', '5dc768b2f067c56f77', '75d295377a46f83236', '2'),
    ('35', '5dc768b2f067c56f77', 'd63386c884aeb9f71d', '8'),
    ('36', '5dc768b2f067c56f77', 'ef5fe98c6b9f313075', '9'),
    ('37', '32ded68d89443e808', '55e60cfcc9dc49c17e', '10'),
    ('38', '32ded68d89443e808', 'e0e0defbb9ec47f6f7', '6'),
    ('39', '32ded68d89443e808', '850badf89ed8f06854', '4'),
    ('40', '32ded68d89443e808', '5eff3a5bfc0687351e', '8'),
    ('41', '32ded68d89443e808', '8bba390b53976da0cd', '6'),
    ('42', '32ded68d89443e808', '91f59516cb9dee1e88', '1'),
    ('43', '32ded68d89443e808', '6edf0be4b2267df1fa', '7'),
    ('44', '32ded68d89443e808', 'd63386c884aeb9f71d', '3'),
    ('45', '32ded68d89443e808', '32ded68d89443e808', '7'),
    ('46', '32ded68d89443e808', '5dc768b2f067c56f77', '9'),
    ('47', '32ded68d89443e808', '406539987dd9b679c0', '3'),
    ('48', '32ded68d89443e808', 'a84065b7933ad01019', '10'),
    ('49', '32ded68d89443e808', '2813e59cf6c1ff698e', '9'),
    ('50', '32ded68d89443e808', 'cbc4bd40cd1687754', '10'),
    ('51', '32ded68d89443e808', 'aa0bd72b729fab6e9e', '4'),
    ('52', '32ded68d89443e808', '75d295377a46f83236', '5'),
    ('53', '32ded68d89443e808', '6b503743a13d778200', '3'),
    ('54', '32ded68d89443e808', '5b8754928306a18b68', '4'),
    ('55', '32ded68d89443e808', '47be2887786891367e', '5'),
    ('56', 'e0e0defbb9ec47f6f7', '5dc768b2f067c56f77', '6'),
    ('57', 'e0e0defbb9ec47f6f7', '2813e59cf6c1ff698e', '4'),
    ('58', 'e0e0defbb9ec47f6f7', '6b503743a13d778200', '8'),
    ('59', 'e0e0defbb9ec47f6f7', 'e22d2eabc2d4c19688', '3'),
    ('60', 'e0e0defbb9ec47f6f7', 'e6088004caf0c8cc51', '2'),
    ('61', 'e0e0defbb9ec47f6f7', 'aa0bd72b729fab6e9e', '6'),
    ('62', 'e0e0defbb9ec47f6f7', '55e60cfcc9dc49c17e', '5'),
    ('63', 'e0e0defbb9ec47f6f7', '850badf89ed8f06854', '6'),
    ('64', 'e0e0defbb9ec47f6f7', 'd63386c884aeb9f71d', '3'),
    ('65', 'e0e0defbb9ec47f6f7', 'a84065b7933ad01019', '10'),
    ('66', 'e0e0defbb9ec47f6f7', '32ded68d89443e808', '6'),
    ('67', 'e0e0defbb9ec47f6f7', '47be2887786891367e', '8'),
    ('68', 'e0e0defbb9ec47f6f7', '157e3e9278e32aba3e', '7'),
    ('69', 'e0e0defbb9ec47f6f7', 'cbc4bd40cd1687754', '2'),
    ('70', 'e0e0defbb9ec47f6f7', 'e0e0defbb9ec47f6f7', '3'),
    ('71', '6b503743a13d778200', '850badf89ed8f06854', '5'),
    ('72', '6b503743a13d778200', '55e60cfcc9dc49c17e', '10'),
    ('73', '6b503743a13d778200', 'cbc4bd40cd1687754', '2'),
    ('74', '6b503743a13d778200', 'e0e0defbb9ec47f6f7', '5'),
    ('75', '6b503743a13d778200', '7cfe354d9a64bf8173', '5'),
    ('76', '6b503743a13d778200', '32ded68d89443e808', '4'),
    ('77', '6b503743a13d778200', 'e6088004caf0c8cc51', '9'),
    ('78', '6b503743a13d778200', 'aa0bd72b729fab6e9e', '7'),
    ('79', '6b503743a13d778200', '5dc768b2f067c56f77', '9'),
    ('80', 'e22d2eabc2d4c19688', '8bba390b53976da0cd', '5'),
    ('81', 'e22d2eabc2d4c19688', 'e0e0defbb9ec47f6f7', '2'),
    ('82', 'e22d2eabc2d4c19688', 'ef5fe98c6b9f313075', '10'),
    ('83', 'e22d2eabc2d4c19688', '5eff3a5bfc0687351e', '2'),
    ('84', 'e22d2eabc2d4c19688', '47be2887786891367e', '4'),
    ('85', 'e22d2eabc2d4c19688', '406539987dd9b679c0', '8'),
    ('86', 'e22d2eabc2d4c19688', 'cbc4bd40cd1687754', '8'),
    ('87', 'e22d2eabc2d4c19688', '7cfe354d9a64bf8173', '10'),
    ('88', 'e22d2eabc2d4c19688', 'e6088004caf0c8cc51', '5'),
    ('89', 'e22d2eabc2d4c19688', 'aa0bd72b729fab6e9e', '9'),
    ('90', 'e22d2eabc2d4c19688', '6edf0be4b2267df1fa', '8'),
    ('91', 'e22d2eabc2d4c19688', '157e3e9278e32aba3e', '3'),
    ('92', 'e22d2eabc2d4c19688', 'd63386c884aeb9f71d', '2'),
    ('93', 'd63386c884aeb9f71d', 'cbc4bd40cd1687754', '6'),
    ('94', 'd63386c884aeb9f71d', '8bba390b53976da0cd', '10'),
    ('95', 'd63386c884aeb9f71d', '75d295377a46f83236', '10'),
    ('96', 'd63386c884aeb9f71d', '5b8754928306a18b68', '4'),
    ('97', 'd63386c884aeb9f71d', 'e6088004caf0c8cc51', '7'),
    ('98', 'd63386c884aeb9f71d', 'e22d2eabc2d4c19688', '9'),
    ('99', 'd63386c884aeb9f71d', '55e60cfcc9dc49c17e', '3'),
    ('100', 'd63386c884aeb9f71d', '5dc768b2f067c56f77', '3'),
    ('101', 'd63386c884aeb9f71d', '32ded68d89443e808', '8'),
    ('102', 'd63386c884aeb9f71d', '157e3e9278e32aba3e', '8'),
    ('103', 'cbc4bd40cd1687754', '7cfe354d9a64bf8173', '10'),
    ('104', 'cbc4bd40cd1687754', '114bafadff2d882864', '2'),
    ('105', 'cbc4bd40cd1687754', '157e3e9278e32aba3e', '4'),
    ('106', 'cbc4bd40cd1687754', 'e6088004caf0c8cc51', '1'),
    ('107', 'cbc4bd40cd1687754', '5eff3a5bfc0687351e', '8'),
    ('108', 'cbc4bd40cd1687754', '5b8754928306a18b68', '5'),
    ('109', 'cbc4bd40cd1687754', '850badf89ed8f06854', '9'),
    ('110', 'cbc4bd40cd1687754', '406539987dd9b679c0', '2'),
    ('111', 'cbc4bd40cd1687754', 'd63386c884aeb9f71d', '4'),
    ('112', 'cbc4bd40cd1687754', 'e0e0defbb9ec47f6f7', '2'),
    ('113', 'cbc4bd40cd1687754', 'cbc4bd40cd1687754', '4'),
    ('114', 'cbc4bd40cd1687754', '2813e59cf6c1ff698e', '5'),
    ('115', 'cbc4bd40cd1687754', '47be2887786891367e', '8'),
    ('116', 'cbc4bd40cd1687754', 'e22d2eabc2d4c19688', '8'),
    ('117', 'cbc4bd40cd1687754', '32ded68d89443e808', '2'),
    ('118', 'ef5fe98c6b9f313075', '47be2887786891367e', '1'),
    ('119', 'ef5fe98c6b9f313075', '91f59516cb9dee1e88', '8'),
    ('120', 'ef5fe98c6b9f313075', '2813e59cf6c1ff698e', '8'),
    ('121', 'ef5fe98c6b9f313075', 'e22d2eabc2d4c19688', '5'),
    ('122', 'ef5fe98c6b9f313075', '406539987dd9b679c0', '8'),
    ('123', 'ef5fe98c6b9f313075', '6b503743a13d778200', '7'),
    ('124', 'ef5fe98c6b9f313075', '850badf89ed8f06854', '1'),
    ('125', 'ef5fe98c6b9f313075', '114bafadff2d882864', '5'),
    ('126', 'ef5fe98c6b9f313075', 'e0e0defbb9ec47f6f7', '9'),
    ('127', 'ef5fe98c6b9f313075', '5dc768b2f067c56f77', '10'),
    ('128', 'ef5fe98c6b9f313075', '55e60cfcc9dc49c17e', '6'),
    ('129', 'ef5fe98c6b9f313075', 'cbc4bd40cd1687754', '1'),
    ('130', 'ef5fe98c6b9f313075', '5b8754928306a18b68', '5'),
    ('131', 'ef5fe98c6b9f313075', '32ded68d89443e808', '10'),
    ('132', 'ef5fe98c6b9f313075', '8bba390b53976da0cd', '10'),
    ('133', 'ef5fe98c6b9f313075', 'd63386c884aeb9f71d', '1'),
    ('134', 'ef5fe98c6b9f313075', 'e6088004caf0c8cc51', '7'),
    ('135', 'ef5fe98c6b9f313075', '5eff3a5bfc0687351e', '8'),
    ('136', 'ef5fe98c6b9f313075', 'ef5fe98c6b9f313075', '7'),
    ('137', '8bba390b53976da0cd', 'd63386c884aeb9f71d', '7'),
    ('138', '8bba390b53976da0cd', '55e60cfcc9dc49c17e', '7'),
    ('139', '8bba390b53976da0cd', '5dc768b2f067c56f77', '6'),
    ('140', '8bba390b53976da0cd', '406539987dd9b679c0', '6'),
    ('141', '8bba390b53976da0cd', '5eff3a5bfc0687351e', '2'),
    ('142', '8bba390b53976da0cd', '91f59516cb9dee1e88', '5'),
    ('143', '8bba390b53976da0cd', 'e6088004caf0c8cc51', '5'),
    ('144', '8bba390b53976da0cd', '75d295377a46f83236', '3'),
    ('145', '8bba390b53976da0cd', 'cbc4bd40cd1687754', '6'),
    ('146', '8bba390b53976da0cd', 'a84065b7933ad01019', '1'),
    ('147', '8bba390b53976da0cd', 'e22d2eabc2d4c19688', '6'),
    ('148', '8bba390b53976da0cd', '6edf0be4b2267df1fa', '1'),
    ('149', '8bba390b53976da0cd', '32ded68d89443e808', '1'),
    ('150', '8bba390b53976da0cd', '850badf89ed8f06854', '8'),
    ('151', '157e3e9278e32aba3e', '850badf89ed8f06854', '3'),
    ('152', '157e3e9278e32aba3e', 'aa0bd72b729fab6e9e', '2'),
    ('153', '157e3e9278e32aba3e', '75d295377a46f83236', '10'),
    ('154', '157e3e9278e32aba3e', 'e22d2eabc2d4c19688', '10'),
    ('155', '157e3e9278e32aba3e', '5eff3a5bfc0687351e', '8'),
    ('156', '157e3e9278e32aba3e', '114bafadff2d882864', '7'),
    ('157', '157e3e9278e32aba3e', '91f59516cb9dee1e88', '4'),
    ('158', '157e3e9278e32aba3e', '6edf0be4b2267df1fa', '3'),
    ('159', '157e3e9278e32aba3e', 'a84065b7933ad01019', '2'),
    ('160', '157e3e9278e32aba3e', '5b8754928306a18b68', '9'),
    ('161', '2813e59cf6c1ff698e', '47be2887786891367e', '2'),
    ('162', '2813e59cf6c1ff698e', 'e0e0defbb9ec47f6f7', '4'),
    ('163', '2813e59cf6c1ff698e', '75d295377a46f83236', '5'),
    ('164', '2813e59cf6c1ff698e', '5eff3a5bfc0687351e', '5'),
    ('165', '2813e59cf6c1ff698e', '406539987dd9b679c0', '7'),
    ('166', '2813e59cf6c1ff698e', 'cbc4bd40cd1687754', '2'),
    ('167', '2813e59cf6c1ff698e', '8bba390b53976da0cd', '3'),
    ('168', '2813e59cf6c1ff698e', '6edf0be4b2267df1fa', '6'),
    ('169', '2813e59cf6c1ff698e', '32ded68d89443e808', '3'),
    ('170', '2813e59cf6c1ff698e', '114bafadff2d882864', '6'),
    ('171', '2813e59cf6c1ff698e', '55e60cfcc9dc49c17e', '2'),
    ('172', 'aa0bd72b729fab6e9e', '75d295377a46f83236', '5'),
    ('173', 'aa0bd72b729fab6e9e', 'e22d2eabc2d4c19688', '10'),
    ('174', 'aa0bd72b729fab6e9e', '91f59516cb9dee1e88', '5'),
    ('175', 'aa0bd72b729fab6e9e', 'ef5fe98c6b9f313075', '6'),
    ('176', 'aa0bd72b729fab6e9e', '8bba390b53976da0cd', '3'),
    ('177', 'aa0bd72b729fab6e9e', 'cbc4bd40cd1687754', '10'),
    ('178', 'aa0bd72b729fab6e9e', '7cfe354d9a64bf8173', '9'),
    ('179', 'aa0bd72b729fab6e9e', '5eff3a5bfc0687351e', '2'),
    ('180', 'aa0bd72b729fab6e9e', '2813e59cf6c1ff698e', '1'),
    ('181', 'aa0bd72b729fab6e9e', 'aa0bd72b729fab6e9e', '1'),
    ('182', 'aa0bd72b729fab6e9e', '47be2887786891367e', '6'),
    ('183', 'aa0bd72b729fab6e9e', 'd63386c884aeb9f71d', '9'),
    ('184', '91f59516cb9dee1e88', 'd63386c884aeb9f71d', '5'),
    ('185', '91f59516cb9dee1e88', 'e6088004caf0c8cc51', '5'),
    ('186', '91f59516cb9dee1e88', '8bba390b53976da0cd', '9'),
    ('187', '91f59516cb9dee1e88', '91f59516cb9dee1e88', '6'),
    ('188', '91f59516cb9dee1e88', 'a84065b7933ad01019', '4'),
    ('189', '91f59516cb9dee1e88', '2813e59cf6c1ff698e', '7'),
    ('190', '91f59516cb9dee1e88', 'aa0bd72b729fab6e9e', '4'),
    ('191', '91f59516cb9dee1e88', '850badf89ed8f06854', '9'),
    ('192', '91f59516cb9dee1e88', '406539987dd9b679c0', '3'),
    ('193', '91f59516cb9dee1e88', 'e22d2eabc2d4c19688', '1'),
    ('194', '91f59516cb9dee1e88', 'ef5fe98c6b9f313075', '7'),
    ('195', '91f59516cb9dee1e88', 'e0e0defbb9ec47f6f7', '5'),
    ('196', '91f59516cb9dee1e88', '6b503743a13d778200', '9'),
    ('197', '91f59516cb9dee1e88', '55e60cfcc9dc49c17e', '4'),
    ('198', '91f59516cb9dee1e88', '6edf0be4b2267df1fa', '3'),
    ('199', '91f59516cb9dee1e88', '5eff3a5bfc0687351e', '8'),
    ('200', '114bafadff2d882864', '157e3e9278e32aba3e', '10'),
    ('201', '114bafadff2d882864', '75d295377a46f83236', '8'),
    ('202', '114bafadff2d882864', '47be2887786891367e', '3'),
    ('203', '114bafadff2d882864', 'aa0bd72b729fab6e9e', '10'),
    ('204', '114bafadff2d882864', 'cbc4bd40cd1687754', '7'),
    ('205', '114bafadff2d882864', '5dc768b2f067c56f77', '2'),
    ('206', '114bafadff2d882864', '2813e59cf6c1ff698e', '5'),
    ('207', '114bafadff2d882864', '5eff3a5bfc0687351e', '6'),
    ('208', '406539987dd9b679c0', 'ef5fe98c6b9f313075', '8'),
    ('209', '406539987dd9b679c0', '114bafadff2d882864', '5'),
    ('210', '406539987dd9b679c0', '8bba390b53976da0cd', '10'),
    ('211', '406539987dd9b679c0', '32ded68d89443e808', '3'),
    ('212', '406539987dd9b679c0', 'e0e0defbb9ec47f6f7', '7'),
    ('213', '406539987dd9b679c0', 'd63386c884aeb9f71d', '7'),
    ('214', '406539987dd9b679c0', '91f59516cb9dee1e88', '6'),
    ('215', '406539987dd9b679c0', 'e22d2eabc2d4c19688', '4'),
    ('216', '406539987dd9b679c0', 'a84065b7933ad01019', '3'),
    ('217', '5eff3a5bfc0687351e', '2813e59cf6c1ff698e', '7'),
    ('218', '5eff3a5bfc0687351e', 'a84065b7933ad01019', '3'),
    ('219', '5eff3a5bfc0687351e', '6b503743a13d778200', '5'),
    ('220', '5eff3a5bfc0687351e', 'ef5fe98c6b9f313075', '1'),
    ('221', '5eff3a5bfc0687351e', '5dc768b2f067c56f77', '2'),
    ('222', '5eff3a5bfc0687351e', 'e6088004caf0c8cc51', '6'),
    ('223', '5eff3a5bfc0687351e', '850badf89ed8f06854', '8'),
    ('224', '5eff3a5bfc0687351e', '91f59516cb9dee1e88', '7'),
    ('225', '5eff3a5bfc0687351e', '75d295377a46f83236', '5'),
    ('226', '5eff3a5bfc0687351e', '47be2887786891367e', '6'),
    ('227', '5eff3a5bfc0687351e', '7cfe354d9a64bf8173', '7'),
    ('228', 'a84065b7933ad01019', '6b503743a13d778200', '3'),
    ('229', 'a84065b7933ad01019', '75d295377a46f83236', '9'),
    ('230', 'a84065b7933ad01019', 'e0e0defbb9ec47f6f7', '6'),
    ('231', 'a84065b7933ad01019', 'a84065b7933ad01019', '6'),
    ('232', 'a84065b7933ad01019', '5dc768b2f067c56f77', '6'),
    ('233', 'a84065b7933ad01019', '2813e59cf6c1ff698e', '1'),
    ('234', 'a84065b7933ad01019', 'd63386c884aeb9f71d', '5'),
    ('235', 'a84065b7933ad01019', 'ef5fe98c6b9f313075', '9'),
    ('236', 'a84065b7933ad01019', '406539987dd9b679c0', '9'),
    ('237', 'a84065b7933ad01019', '55e60cfcc9dc49c17e', '2'),
    ('238', 'a84065b7933ad01019', '8bba390b53976da0cd', '9'),
    ('239', 'a84065b7933ad01019', '850badf89ed8f06854', '9'),
    ('240', 'a84065b7933ad01019', 'e6088004caf0c8cc51', '5'),
    ('241', 'a84065b7933ad01019', '157e3e9278e32aba3e', '9'),
    ('242', '850badf89ed8f06854', 'e22d2eabc2d4c19688', '4'),
    ('243', '850badf89ed8f06854', '91f59516cb9dee1e88', '9'),
    ('244', '850badf89ed8f06854', '32ded68d89443e808', '4'),
    ('245', '850badf89ed8f06854', '5dc768b2f067c56f77', '10'),
    ('246', '850badf89ed8f06854', 'cbc4bd40cd1687754', '3'),
    ('247', '850badf89ed8f06854', 'aa0bd72b729fab6e9e', '4'),
    ('248', '850badf89ed8f06854', '7cfe354d9a64bf8173', '5'),
    ('249', '850badf89ed8f06854', 'a84065b7933ad01019', '5'),
    ('250', '850badf89ed8f06854', '5b8754928306a18b68', '1'),
    ('251', '850badf89ed8f06854', '157e3e9278e32aba3e', '4'),
    ('252', '850badf89ed8f06854', '55e60cfcc9dc49c17e', '4'),
    ('253', '850badf89ed8f06854', '850badf89ed8f06854', '9'),
    ('254', '47be2887786891367e', 'e0e0defbb9ec47f6f7', '5'),
    ('255', '47be2887786891367e', '2813e59cf6c1ff698e', '9'),
    ('256', '47be2887786891367e', '55e60cfcc9dc49c17e', '7'),
    ('257', '47be2887786891367e', '406539987dd9b679c0', '7'),
    ('258', '47be2887786891367e', 'e22d2eabc2d4c19688', '4'),
    ('259', '47be2887786891367e', 'aa0bd72b729fab6e9e', '6'),
    ('260', '55e60cfcc9dc49c17e', 'd63386c884aeb9f71d', '2'),
    ('261', '55e60cfcc9dc49c17e', '6edf0be4b2267df1fa', '4'),
    ('262', '55e60cfcc9dc49c17e', '5eff3a5bfc0687351e', '5'),
    ('263', '55e60cfcc9dc49c17e', '850badf89ed8f06854', '3'),
    ('264', '55e60cfcc9dc49c17e', '47be2887786891367e', '8'),
    ('265', '55e60cfcc9dc49c17e', '75d295377a46f83236', '7'),
    ('266', '55e60cfcc9dc49c17e', '5b8754928306a18b68', '8'),
    ('267', '55e60cfcc9dc49c17e', '114bafadff2d882864', '7'),
    ('268', '55e60cfcc9dc49c17e', 'e0e0defbb9ec47f6f7', '5'),
    ('269', '55e60cfcc9dc49c17e', 'e6088004caf0c8cc51', '1'),
    ('270', '55e60cfcc9dc49c17e', '32ded68d89443e808', '10'),
    ('271', '55e60cfcc9dc49c17e', 'ef5fe98c6b9f313075', '8'),
    ('272', '55e60cfcc9dc49c17e', '2813e59cf6c1ff698e', '3'),
    ('273', '55e60cfcc9dc49c17e', 'aa0bd72b729fab6e9e', '6'),
    ('274', '55e60cfcc9dc49c17e', '406539987dd9b679c0', '1'),
    ('275', '55e60cfcc9dc49c17e', '6b503743a13d778200', '1'),
    ('276', '5b8754928306a18b68', '7cfe354d9a64bf8173', '1'),
    ('277', '5b8754928306a18b68', '6edf0be4b2267df1fa', '1'),
    ('278', '5b8754928306a18b68', 'cbc4bd40cd1687754', '6'),
    ('279', '5b8754928306a18b68', '406539987dd9b679c0', '1'),
    ('280', '5b8754928306a18b68', '5dc768b2f067c56f77', '4'),
    ('281', '5b8754928306a18b68', 'a84065b7933ad01019', '4'),
    ('282', '5b8754928306a18b68', '6b503743a13d778200', '10'),
    ('283', '5b8754928306a18b68', '2813e59cf6c1ff698e', '9'),
    ('284', '5b8754928306a18b68', 'e0e0defbb9ec47f6f7', '4'),
    ('285', '5b8754928306a18b68', 'e6088004caf0c8cc51', '5'),
    ('286', '5b8754928306a18b68', '91f59516cb9dee1e88', '8'),
    ('287', '5b8754928306a18b68', '32ded68d89443e808', '3'),
    ('288', '5b8754928306a18b68', 'd63386c884aeb9f71d', '3'),
    ('289', '5b8754928306a18b68', '55e60cfcc9dc49c17e', '2'),
    ('290', '5b8754928306a18b68', 'aa0bd72b729fab6e9e', '8'),
    ('291', '5b8754928306a18b68', '5b8754928306a18b68', '3'),
    ('292', '5b8754928306a18b68', '157e3e9278e32aba3e', '7'),
    ('293', '5b8754928306a18b68', '5eff3a5bfc0687351e', '5'),
    ('294', '7cfe354d9a64bf8173', 'ef5fe98c6b9f313075', '4'),
    ('295', '7cfe354d9a64bf8173', 'd63386c884aeb9f71d', '7'),
    ('296', '7cfe354d9a64bf8173', '5eff3a5bfc0687351e', '1'),
    ('297', '7cfe354d9a64bf8173', '6b503743a13d778200', '7'),
    ('298', '7cfe354d9a64bf8173', '47be2887786891367e', '1'),
    ('299', '7cfe354d9a64bf8173', '32ded68d89443e808', '4'),
    ('300', '7cfe354d9a64bf8173', '6edf0be4b2267df1fa', '9'),
    ('301', '7cfe354d9a64bf8173', '5b8754928306a18b68', '9'),
    ('302', '7cfe354d9a64bf8173', 'e22d2eabc2d4c19688', '9'),
    ('303', '7cfe354d9a64bf8173', 'cbc4bd40cd1687754', '8'),
    ('304', '7cfe354d9a64bf8173', '2813e59cf6c1ff698e', '6'),
    ('305', '7cfe354d9a64bf8173', 'e6088004caf0c8cc51', '6'),
    ('306', '7cfe354d9a64bf8173', '157e3e9278e32aba3e', '1'),
    ('307', '7cfe354d9a64bf8173', '406539987dd9b679c0', '3'),
    ('308', '7cfe354d9a64bf8173', '91f59516cb9dee1e88', '7'),
    ('309', 'e6088004caf0c8cc51', '5dc768b2f067c56f77', '7'),
    ('310', 'e6088004caf0c8cc51', 'cbc4bd40cd1687754', '4'),
    ('311', 'e6088004caf0c8cc51', '6edf0be4b2267df1fa', '9'),
    ('312', 'e6088004caf0c8cc51', 'aa0bd72b729fab6e9e', '4'),
    ('313', 'e6088004caf0c8cc51', '47be2887786891367e', '2'),
    ('314', 'e6088004caf0c8cc51', 'e6088004caf0c8cc51', '5');

    -- rank users by the total number of sent emails

SELECT from_user, COUNT(from_user) AS total_emails,
    RANK() OVER(
            ORDER BY COUNT(from_user) DESC, 
                    from_user ASC) AS activity_rank
FROM google_gmail_emails
GROUP BY from_user
ORDER BY total_emails DESC;


-- MEDIUM
-- SOURCE: https://platform.stratascratch.com/coding-question?id=10315&python=
-- Cities With The Most Expensive Homes
-- Write a query that identifies cities with higher than average home 
-- prices when compared to the national average. Output the city names.

CREATE TABLE zillow_transactions
(
     id     varchar(3),
     state  varchar(2),
     city   varchar(20),
     street_address     varchar(50),
     mkt_price  int(9)
);

INSERT INTO zillow_transactions ( id , state , city , street_address , mkt_price )
VALUES
    ('1', 'NY', 'New York City', '66 Trout Drive', 449761),
    ('2', 'NY', 'New York City', 'Atwater', 277527),
    ('3', 'NY', 'New York City', '58 Gates Street', 268394),
    ('4', 'NY', 'New York City', 'Norcross', 279929),
    ('5', 'NY', 'New York City', '337 Shore Ave.', 151592),
    ('6', 'NY', 'New York City', 'Plainfield', 624531),
    ('7', 'NY', 'New York City', '84 Central Street', 267345),
    ('8', 'NY', 'New York City', 'Passaic', 88504),
    ('9', 'NY', 'New York City', '951 Fulton Road', 270476),
    ('10', 'NY', 'New York City', 'Oxon Hill', 118112),
    ('11', 'CA', 'Los Angeles', '692 Redwood Court', 150707),
    ('12', 'CA', 'Los Angeles', 'Lewiston', 463180),
    ('13', 'CA', 'Los Angeles', '8368 West Acacia Ave.', 538865),
    ('14', 'CA', 'Los Angeles', 'Pearl', 390896),
    ('15', 'CA', 'Los Angeles', '8206 Old Riverview Rd.', 117754),
    ('16', 'CA', 'Los Angeles', 'Seattle', 424588),
    ('17', 'CA', 'Los Angeles', '7227 Joy Ridge Rd.', 156850),
    ('18', 'CA', 'Los Angeles', 'Battle Ground', 643454),
    ('19', 'CA', 'Los Angeles', '233 Bedford Ave.', 713841),
    ('20', 'CA', 'Los Angeles', 'Saint Albans', 295852),
    ('21', 'IL', 'Chicago', '8830 Baker St.', 12944),
    ('22', 'IL', 'Chicago', 'Watertown', 410766),
    ('23', 'IL', 'Chicago', '632 Princeton St.', 160696),
    ('24', 'IL', 'Chicago', 'Waxhaw', 464144),
    ('25', 'IL', 'Chicago', '7773 Tailwater Drive', 129393),
    ('26', 'IL', 'Chicago', 'Bonita Springs', 174886),
    ('27', 'IL', 'Chicago', '31 Summerhouse Rd.', 296008),
    ('28', 'IL', 'Chicago', 'Middleburg', 279000),
    ('29', 'IL', 'Chicago', '273 Windfall Avenue', 424846),
    ('30', 'IL', 'Chicago', 'Graham', 592268),
    ('31', 'TX', 'Houston', '91 Canterbury Dr.', 632014),
    ('32', 'TX', 'Houston', 'Dallas', 68868),
    ('33', 'TX', 'Houston', '503 Elmwood St.', 454184),
    ('34', 'TX', 'Houston', 'Kennewick', 186280),
    ('35', 'TX', 'Houston', '739 Chapel Street', 334474),
    ('36', 'TX', 'Houston', 'San Angelo', 204460),
    ('37', 'TX', 'Houston', '572 Parker Dr.', 678443),
    ('38', 'TX', 'Houston', 'Bellmore', 401090),
    ('39', 'TX', 'Houston', '8653 South Oxford Street', 482214),
    ('40', 'TX', 'Houston', 'Butler', 330868),
    ('41', 'AZ', 'Phoenix', '8667 S. Joy Ridge Court', 316291),
    ('42', 'AZ', 'Phoenix', 'Torrance', 210392),
    ('43', 'AZ', 'Phoenix', '35 Harvard St.', 167502),
    ('44', 'AZ', 'Phoenix', 'Nutley', 327554),
    ('45', 'AZ', 'Phoenix', '7313 Vermont St.', 285135),
    ('46', 'AZ', 'Phoenix', 'Lemont', 577667),
    ('47', 'AZ', 'Phoenix', '8905 Buttonwood Dr.', 212301),
    ('48', 'AZ', 'Phoenix', 'Lafayette', 317504),
    ('49', 'AZ', 'Phoenix', '170 Brandywine Drive', 287864),
    ('50', 'AZ', 'Phoenix', 'Lake Mary', 822532),
    ('51', 'PA', 'Philadelphia', '77 West Mayfield St.', 541423),
    ('52', 'PA', 'Philadelphia', 'Port Chester', 334229),
    ('53', 'PA', 'Philadelphia', '92 Bedford Lane', 445591),
    ('54', 'PA', 'Philadelphia', 'Palm Bay', 461683),
    ('55', 'PA', 'Philadelphia', '86 Edgemont Drive', 386885),
    ('56', 'PA', 'Philadelphia', 'Hollywood', 670135),
    ('57', 'PA', 'Philadelphia', '946 George St.', 416003),
    ('58', 'PA', 'Philadelphia', 'Braintree', 477100),
    ('59', 'PA', 'Philadelphia', '35 Sussex Court', 293291),
    ('60', 'PA', 'Philadelphia', 'Henderson', 384877),
    ('61', 'CA', 'San Diego', '222 N. Tunnel Ave.', 140981),
    ('62', 'CA', 'San Diego', 'Orange', 465564),
    ('63', 'CA', 'San Diego', '757 Lakewood Street', 235446),
    ('64', 'CA', 'San Diego', 'Ronkonkoma', 143486),
    ('65', 'CA', 'San Diego', '562 Marlborough Drive', 609823),
    ('66', 'CA', 'San Diego', 'Fitchburg', 202015),
    ('67', 'CA', 'San Diego', '7487 North Market Drive', 455428),
    ('68', 'CA', 'San Diego', 'El Paso', 252083),
    ('69', 'CA', 'San Diego', '672 High Ridge Drive', 506868),
    ('70', 'CA', 'San Diego', 'Great Falls', 90000),
    ('71', 'CA', 'San Francisco', '9936 SW. Livingston Street', 217451),
    ('72', 'CA', 'San Francisco', 'Clearwater', 1794282),
    ('73', 'CA', 'San Francisco', '76 South Ave.', 1692646),
    ('74', 'CA', 'San Francisco', 'Bethel Park', 596998),
    ('75', 'CA', 'San Francisco', '51 Homestead Ave.', 1495620),
    ('76', 'CA', 'San Francisco', 'Valparaiso', 1752703),
    ('77', 'CA', 'San Francisco', '7851 Lookout Court', 1383822),
    ('78', 'CA', 'San Francisco', 'Seymour', 243464),
    ('79', 'CA', 'San Francisco', '8290 Bradford Street', 1568759),
    ('80', 'CA', 'San Francisco', 'Sugar Land', 28285),
    ('81', 'CA', 'Santa Clara', '9673 South Homewood St.', 1119747),
    ('82', 'CA', 'Santa Clara', 'Mount Juliet', 204832),
    ('83', 'CA', 'Santa Clara', '73 East South Ave.', 917268),
    ('84', 'CA', 'Santa Clara', 'Hudson', 713266),
    ('85', 'CA', 'Santa Clara', '669 South Columbia St.', 974397),
    ('86', 'CA', 'Santa Clara', 'Hummelstown', 944574),
    ('87', 'CA', 'Santa Clara', '7699 S. Orange St.', 1100325),
    ('88', 'CA', 'Santa Clara', 'Asbury Park', 966673),
    ('89', 'CA', 'Santa Clara', '19 Beacon Circle', 1207658),
    ('90', 'CA', 'Santa Clara', 'Phillipsburg', 1682459),
    ('91', 'CA', 'Mountain View', '730 El Dorado Street', 893838),
    ('92', 'CA', 'Mountain View', 'Bear', 446689),
    ('93', 'CA', 'Mountain View', '8233 Swanson Circle', 1271223),
    ('94', 'CA', 'Mountain View', 'Lumberton', 949895),
    ('95', 'CA', 'Mountain View', '8803 University Drive', 1376870),
    ('96', 'CA', 'Mountain View', 'Sterling Heights', 1235082),
    ('97', 'CA', 'Mountain View', '9447 Rockwell St.', 1569111),
    ('98', 'CA', 'Mountain View', 'Biloxi', 853914),
    ('99', 'CA', 'Mountain View', '219 East Rock Maple Ave.', 746615),
    ('100', 'CA', 'Mountain View', 'Bridgewater', 616129);


SELECT a.city, AVG(a.mkt_price)
FROM zillow_transactions a
GROUP BY a.city
HAVING AVG(a.mkt_price) > (SELECT AVG(mkt_price) FROM zillow_transactions)
ORDER BY a.city ASC
;